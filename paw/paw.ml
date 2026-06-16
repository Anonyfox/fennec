(** Paw — a small, complete HTTP toolkit for OCaml/Eio. This module is the whole
    public surface; the implementation is organized in folders (http/, conn/,
    pipeline/, ws/, dev/). A paw touches a connection ([Conn.t -> Conn.t]); compose
    with {!seq}, the first to answer wins. *)

include Pipeline

(** {1 The connection} *)
module Conn = Conn

module Assigns = Assigns

(** {1 HTTP vocabulary} *)
module Http = Http

module Headers = Headers
module Cookie = Cookie
module Mime = Mime
module Multipart = Multipart
module Http_date = Http_date
module Http_semantics = Http_semantics

(** {1 WebSockets & dev} *)
module Ws_channel = Ws_channel

module Dev = Dev
module Dev_proto = Dev_proto
