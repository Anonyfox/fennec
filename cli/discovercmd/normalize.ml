let is_word = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

let split_camel s =
  let b = Buffer.create (String.length s + 8) in
  String.iteri
    (fun i c ->
      if
        i > 0
        && c >= 'A'
        && c <= 'Z'
        &&
        match s.[i - 1] with
        | 'a' .. 'z' | '0' .. '9' -> true
        | _ -> false
      then Buffer.add_char b ' ';
      Buffer.add_char b c)
    s;
  Buffer.contents b

let normalize_char = function
  | '_' | '-' | '/' | ':' | '(' | ')' | '[' | ']' | '{' | '}' | ',' | ';' | '.' -> ' '
  | c when is_word c -> Char.lowercase_ascii c
  | _ -> ' '

let words_uncached s =
  s |> split_camel |> String.map normalize_char |> String.split_on_char ' '
  |> List.filter (fun s -> s <> "")
  |> List.map (fun s ->
         let len = String.length s in
         if len > 4 && s.[len - 1] = 's' then String.sub s 0 (len - 1) else s)
  |> List.filter (fun s ->
         not
           (List.mem s
              [
                "a";
                "an";
                "and";
                "the";
                "to";
                "for";
                "of";
                "in";
                "on";
                "with";
                "from";
                "my";
                "do";
                "i";
                "how";
                "where";
                "what";
                "build";
                "make";
                "add";
                "use";
              ]))

let words_cache : (string, string list) Hashtbl.t = Hashtbl.create 4096

let words s =
  match Hashtbl.find_opt words_cache s with
  | Some words -> words
  | None ->
    let words = words_uncached s in
    if Hashtbl.length words_cache < 20000 then Hashtbl.add words_cache s words;
    words

let uniq xs = List.sort_uniq String.compare xs

let query s = uniq (words s)

let contains_word ~word text = List.exists (( = ) word) (words text)

let%test "normalizes camel and snake" =
  words "Basic_auth SessionCookie" = [ "basic"; "auth"; "session"; "cookie" ]

let%test "keeps acronyms as words" =
  words "HTTP SSR DDP" = [ "http"; "ssr"; "ddp" ]
