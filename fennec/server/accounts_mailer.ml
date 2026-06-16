module Mail = Fennec_mail

type rendered = { subject : string; text : string; html : Fur.vnode option }

(* a template renders the email for one account flow. It takes only primitives — the configured site name,
   the recipient address, and the action URL — so this module sits BELOW accounts core (no dependency on
   the user record), and an app can override any field. *)
type template = site_name:string -> email:string -> url:string -> rendered

type templates = {
  site_name : string;
  from : Mail.Address.t;
  verify_email : template;
  reset_password : template;
  enroll_account : template;
}

(* ---- the default Fur templates: one clean, email-safe layout (inline styles, no external CSS) ---- *)

let layout ~site_name ~heading ~intro ~cta_label ~url ~footer =
  Fur.h "div"
    [ Fur.attr "style" "font-family:system-ui,-apple-system,Arial,sans-serif;max-width:480px;margin:0 auto;padding:24px;color:#1a1a1a" ]
    [ Fur.h "h1" [ Fur.attr "style" "font-size:20px;font-weight:600;margin:0 0 16px" ] [ Fur.text heading ];
      Fur.h "p" [ Fur.attr "style" "font-size:15px;line-height:1.5;margin:0 0 20px" ] [ Fur.text intro ];
      Fur.h "p"
        [ Fur.attr "style" "margin:0 0 24px" ]
        [ Fur.h "a"
            [ Fur.attr "href" url;
              Fur.attr "style"
                "display:inline-block;background:#2563eb;color:#ffffff;text-decoration:none;padding:10px 18px;border-radius:6px;font-size:15px"
            ]
            [ Fur.text cta_label ]
        ];
      Fur.h "p" [ Fur.attr "style" "font-size:13px;color:#555;line-height:1.5;margin:0 0 4px" ] [ Fur.text footer ];
      Fur.h "p"
        [ Fur.attr "style" "font-size:13px;color:#888;word-break:break-all;margin:0 0 16px" ]
        [ Fur.text url ];
      Fur.h "p" [ Fur.attr "style" "font-size:12px;color:#aaa;margin:0" ] [ Fur.text ("— " ^ site_name) ]
    ]

let default_verify : template =
 fun ~site_name ~email:_ ~url ->
  { subject = "Verify your email for " ^ site_name;
    text =
      Printf.sprintf
        "Welcome to %s!\n\nConfirm your email address by opening this link:\n%s\n\nIf you did not create this account, you can safely ignore this message.\n"
        site_name url;
    html =
      Some
        (layout ~site_name ~heading:"Verify your email"
           ~intro:(Printf.sprintf "Confirm your email address to finish setting up your %s account." site_name)
           ~cta_label:"Verify email" ~url
           ~footer:"If you did not create this account, you can safely ignore this email.")
  }

let default_reset : template =
 fun ~site_name ~email:_ ~url ->
  { subject = Printf.sprintf "Reset your %s password" site_name;
    text =
      Printf.sprintf
        "We received a request to reset your %s password.\n\nReset it here:\n%s\n\nIf you did not request this, you can ignore this message — your password will not change.\n"
        site_name url;
    html =
      Some
        (layout ~site_name ~heading:"Reset your password"
           ~intro:(Printf.sprintf "We received a request to reset your %s password." site_name)
           ~cta_label:"Reset password" ~url
           ~footer:"If you did not request this, you can ignore this email — your password will not change.")
  }

let default_enroll : template =
 fun ~site_name ~email:_ ~url ->
  { subject = Printf.sprintf "You're invited to %s" site_name;
    text =
      Printf.sprintf "You have been invited to %s.\n\nSet your password to activate your account:\n%s\n"
        site_name url;
    html =
      Some
        (layout ~site_name ~heading:(Printf.sprintf "Welcome to %s" site_name)
           ~intro:"You have been invited. Set a password to activate your account."
           ~cta_label:"Set your password" ~url ~footer:"This invitation link will expire after a while.")
  }

let default ?(site_name = "Fennec") ~from () =
  { site_name; from; verify_email = default_verify; reset_password = default_reset; enroll_account = default_enroll }

(* render a chosen template into a ready-to-send message (html via Fur.to_html, text as-is) *)
let message ~from ~site_name (tpl : template) ~email ~url =
  let r = tpl ~site_name ~email ~url in
  Mail.make ~from ~to_:[ Mail.Address.v email ] ~subject:r.subject ~text:r.text
    ?html:(Option.map Fur.to_html r.html) ()
