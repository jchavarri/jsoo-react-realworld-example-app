open Js_of_ocaml

let%component make ~(id : string) ~children =
  React.cloneElement children (Js.Unsafe.obj [| "data-testid", Js.Unsafe.inject (Js.string id) |])
