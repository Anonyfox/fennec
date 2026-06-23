(** Generate the modern minimal favicon set from one source image, plus the HTML and web-manifest to
    wire it up — the common "favicons are overcomplicated" chore, in one call. *)

(** One generated icon: its filename (relative to the output dir), square pixel size, and format. *)
type icon = {
  filename : string;
  size : int;
  format : Format.t;
}

(** The set produced from the source: [favicon.ico] (32px), [apple-touch-icon.png] (180px), and the PWA
    [icon-192.png] / [icon-512.png]. Each is a centre-cropped square. *)
val set : icon list

(** The [<link>] tags to drop into a page's [<head>]. *)
val html_snippet : string

(** The [manifest.webmanifest] contents (the 192 + 512 "any maskable" icons). *)
val manifest_json : string

(** [generate ~input ~dir] writes every {!set} icon + the manifest into [dir] (created if missing), each
    centre-cropped and encoded from [input]. Returns {!html_snippet} for the caller to print, or
    [Error msg] on the first decode/encode/I-O failure. *)
val generate : input:bytes -> dir:string -> (string, string) result
