(* Extract the SNI server-name from a TLS ClientHello record — for on-demand TLS, where the cert to
   present is chosen by the requested hostname before the handshake. Pure, read-only, and fail-safe:
   any malformation (truncation, unexpected bytes) yields [None] rather than raising, so a peek that
   can't find a name simply falls back to the normal handshake. The ClientHello wire format is stable
   across TLS 1.0–1.3, so this needs no protocol library. *)

(* the structure parsed (all lengths big-endian):
   record:   type(1)=22  version(2)  length(2)  payload…
   payload:  hsType(1)=1  length(3)  version(2)  random(32)
             sessionId: len(1) + bytes
             cipherSuites: len(2) + bytes
             compression: len(1) + bytes
             extensions: len(2) + [ type(2) len(2) data ]…
   server_name ext (type 0): list_len(2)  [ nameType(1)=0  len(2)  host ] *)
let host_of_client_hello (buf : string) : string option =
  let n = String.length buf in
  let byte i = if i >= 0 && i < n then Char.code buf.[i] else -1 in
  let u16 i = let a = byte i and b = byte (i + 1) in if a < 0 || b < 0 then -1 else (a lsl 8) lor b in
  if byte 0 <> 22 || byte 5 <> 1 then None (* not a handshake record / not a ClientHello *)
  else
    let p = 5 + 4 + 2 + 32 in (* skip hsType+len(4), version(2), random(32) *)
    let sid = byte p in
    if sid < 0 then None
    else
      let p = p + 1 + sid in
      let cs = u16 p in
      if cs < 0 then None
      else
        let p = p + 2 + cs in
        let comp = byte p in
        if comp < 0 then None
        else
          let p = p + 1 + comp in
          let ext_total = u16 p in
          if ext_total < 0 then None
          else
            let p = p + 2 in
            let ext_end = min n (p + ext_total) in
            let rec walk p =
              if p + 4 > ext_end then None
              else
                let etype = u16 p and elen = u16 (p + 2) in
                if etype = 0 then (
                  (* server_name extension: skip list_len(2) + nameType(1); read name len(2) + host *)
                  let q = p + 4 in
                  let name_len = u16 (q + 3) in
                  if byte (q + 2) = 0 && name_len > 0 && q + 5 + name_len <= n then Some (String.sub buf (q + 5) name_len) else None)
                else walk (p + 4 + elen)
            in
            walk p

(* ──── sni tests ──── *)

(* build a minimal but well-formed ClientHello carrying [host] in the SNI extension, so the parser
   is tested against a correct wire shape (round-trip), plus the fail-safe paths *)
let build_client_hello host =
  let b = Buffer.create 96 in
  let u16 v = Buffer.add_char b (Char.chr ((v lsr 8) land 0xff)); Buffer.add_char b (Char.chr (v land 0xff)) in
  let hl = String.length host in
  let sni_entry = 1 + 2 + hl in           (* nameType(1) + len(2) + host *)
  let sni_data = 2 + sni_entry in          (* list_len(2) + entry *)
  let ext = 2 + 2 + sni_data in            (* extType(2) + extLen(2) + data *)
  let body = 2 + 32 + 1 + 2 + 2 + 1 + 1 + 2 + ext in (* ver+random+sid+suites+comp+extLen+ext *)
  Buffer.add_char b '\x16'; u16 0x0301; u16 (4 + body);    (* record: handshake, len *)
  Buffer.add_char b '\x01';                                 (* hsType ClientHello *)
  Buffer.add_char b '\x00'; u16 body;                       (* hs len (3 bytes: 0 + u16) *)
  u16 0x0303;                                               (* client version *)
  Buffer.add_string b (String.make 32 '\x00');             (* random *)
  Buffer.add_char b '\x00';                                 (* session id len 0 *)
  u16 2; u16 0x1301;                                        (* cipher suites *)
  Buffer.add_char b '\x01'; Buffer.add_char b '\x00';       (* compression: null *)
  u16 ext;                                                  (* extensions total len *)
  u16 0; u16 sni_data;                                      (* ext type 0 (server_name), ext len *)
  u16 sni_entry; Buffer.add_char b '\x00'; u16 hl; Buffer.add_string b host; (* list, host entry *)
  Buffer.contents b

let%test "SNI parser extracts the hostname from a ClientHello (round-trip)" =
  host_of_client_hello (build_client_hello "tenant.example.com") = Some "tenant.example.com"
  && host_of_client_hello (build_client_hello "a.io") = Some "a.io"

let%test "SNI parser is fail-safe on junk / truncation / non-handshake" =
  host_of_client_hello "" = None
  && host_of_client_hello "\x17\x03\x03\x00\x01x" = None (* not a handshake record *)
  && host_of_client_hello (String.sub (build_client_hello "x.com") 0 20) = None (* truncated *)
