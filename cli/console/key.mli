(** Decode raw terminal input bytes into editing keys.

    A terminal delivers control keys and arrows as single bytes or short escape sequences (e.g. Up is
    [ESC \[ A]). {!parse} turns a chunk of received bytes into the keys the line editor understands. It
    is stateless per call — a chunk from a single [read] holds whole sequences in practice; a lone
    trailing [ESC] is reported as {!Esc}. *)

type t =
  | Char of char  (** a printable / pass-through byte *)
  | Enter
  | Backspace
  | Tab
  | Delete  (** forward-delete *)
  | Left
  | Right
  | Up
  | Down
  | Home
  | End
  | Ctrl of char  (** Ctrl + a letter, the letter lower-cased (e.g. [Ctrl 'c']) *)
  | Esc
  | Unknown

(** [parse bytes] decodes one chunk of terminal input into keys, in order. *)
val parse : string -> t list
