open Js_of_ocaml

type t =
  | Fetch of (int * string * [ `text of string | `json of Js.json ])
  | Decode of string

let fetch : int * string * [ `text of string | `json of Js.json ] -> t = fun e -> Fetch e

let decode (result : ('a, string) result) : ('a, t) result =
  match result with
  | Ok _ok as ok -> ok
  | Error err -> Error (Decode err)
