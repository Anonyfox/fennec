(* Bridge Mail through the effects outbox — invisible machinery, installed by {!Fennec.serve}. A
   [Mail.send] made inside a workflow body (a transaction is open) or a reaction ([@after]) is recorded
   in the outbox and delivered durably + exactly-once by the worker; a send made anywhere else (a request
   handler, accounts) is sent inline, unchanged. This lives in the app layer because it joins Mail (a
   transport) and the workflow outbox (orchestration) — neither depends on the other; the bridge is two
   refs the framework wires at boot, so the layering stays clean. *)

module B = Bson
module Mail = Fennec_mail
module Outbox = Fennec_pulse_workflow.Outbox
module Workflow = Fennec_pulse_workflow.Workflow
module Tx = Fennec_mongo_dynamic.Tx

(* ---- Mail.t <-> Bson: the durable outbox payload (the worker re-sends from this after a crash) ---- *)

let addr_to_bson (a : Mail.Address.t) =
  B.doc (("email", B.str a.Mail.Address.email) :: (match a.Mail.Address.name with Some n -> [ ("name", B.str n) ] | None -> []))

let addr_of_bson d = Mail.Address.v ?name:(B.get_string d "name") (Option.value ~default:"" (B.get_string d "email"))
let addrs_to_bson l = B.Array (List.map addr_to_bson l)
let addrs_of_bson = function B.Array l -> List.map addr_of_bson l | _ -> []
let opt_str = function Some s -> B.str s | None -> B.Null
let str_opt = function B.String s -> Some s | _ -> None

let mail_to_bson (m : Mail.t) : B.t =
  B.doc
    [ ("from", addr_to_bson m.Mail.from); ("to", addrs_to_bson m.Mail.to_); ("cc", addrs_to_bson m.Mail.cc);
      ("bcc", addrs_to_bson m.Mail.bcc);
      ("reply_to", (match m.Mail.reply_to with Some a -> addr_to_bson a | None -> B.Null));
      ("subject", B.str m.Mail.subject); ("text", opt_str m.Mail.text); ("html", opt_str m.Mail.html);
      ("headers", B.Array (List.map (fun (k, v) -> B.doc [ ("k", B.str k); ("v", B.str v) ]) m.Mail.headers));
      ( "attachments",
        B.Array
          (List.map
             (fun (a : Mail.attachment) ->
               B.doc [ ("filename", B.str a.Mail.filename); ("content_type", B.str a.Mail.content_type); ("content", B.str a.Mail.content) ])
             m.Mail.attachments) ) ]

let mail_of_bson (d : B.t) : Mail.t =
  let get k = Option.value ~default:B.Null (B.get d k) in
  let headers =
    match get "headers" with
    | B.Array l -> List.filter_map (fun h -> match (B.get_string h "k", B.get_string h "v") with Some k, Some v -> Some (k, v) | _ -> None) l
    | _ -> []
  in
  let attachments =
    match get "attachments" with
    | B.Array l ->
      List.filter_map
        (fun a ->
          match (B.get_string a "filename", B.get_string a "content_type", B.get_string a "content") with
          | Some f, Some ct, Some c -> Some { Mail.filename = f; content_type = ct; content = c }
          | _ -> None)
        l
    | _ -> []
  in
  Mail.make ~from:(addr_of_bson (get "from")) ~to_:(addrs_of_bson (get "to")) ~cc:(addrs_of_bson (get "cc"))
    ~bcc:(addrs_of_bson (get "bcc"))
    ?reply_to:(match get "reply_to" with B.Document _ as a -> Some (addr_of_bson a) | _ -> None)
    ~subject:(Option.value ~default:"" (B.get_string d "subject"))
    ?text:(str_opt (get "text")) ?html:(str_opt (get "html")) ~headers ~attachments ()

(* ---- the bridge ---- *)

(* [install ~transport ()] registers the outbox's mail deliverer (the real transport, run directly — no
   re-defer, no second dev-capture) and the deferred-send gate. A transport [Error] RAISES so the outbox
   retries; a permanently-bad message dead-letters after the worker's attempt cap. *)
let install ~(transport : Mail.transport) () =
  Outbox.register ~kind:"mail" (fun ~id:_ payload ->
      match transport (mail_of_bson payload) with Ok () -> () | Error e -> failwith (Mail.string_of_error e));
  Mail.set_deferred_send (fun m ->
      if Tx.current () <> None || Workflow.in_effect () then (
        Outbox.enqueue ~kind:"mail" ~payload:(mail_to_bson m);
        true)
      else false)
