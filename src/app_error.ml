open Js_of_ocaml

type 'a t =
  | Fetch of (int * string * [ `text of string | `json of 'a Js.t ])
  | Decode of string

let fetch : int * string * [ `text of string | `json of 'a Js.t ] -> 'a t = fun e -> Fetch e

let decode (result : ('a, string) result) : ('a, 'b t) result =
  match result with
  | Ok _ok as ok -> ok
  | Error err ->
    Js_of_ocaml.Firebug.console##log_2 "error" result;
    Error (Decode err)
