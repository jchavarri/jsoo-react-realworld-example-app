type domPurify

val marked : string -> string
  [@@js.custom
    type marked_import = Ojs.t

    let marked_import_to_js v = v

    let marked : marked_import = Js_of_ocaml.Js.Unsafe.js_expr {|require("marked")|}

    val marked_internal : marked_import -> string -> string [@@js.call "marked"]

    let marked str = marked_internal marked str]

val create_domPurify : Js_of_ocaml.Dom_html.window Js_of_ocaml.Js.t -> domPurify
  [@@js.custom
    let domPurify_import_to_js v = v

    type window = Js_of_ocaml.Dom_html.window Js_of_ocaml.Js.t

    external window_to_js : Js_of_ocaml.Dom_html.window Js_of_ocaml.Js.t -> Ojs.t = "%identity"

    let domPurify : window -> domPurify = Js_of_ocaml.Js.Unsafe.js_expr {|require("dompurify")|}

    let create_domPurify window = domPurify window]

val sanitize : domPurify -> string -> string [@@js.call "sanitize"]

val to_html : string -> string
  [@@js.custom
    let dompurify = create_domPurify Js_of_ocaml.Dom_html.window

    let to_html markdown = sanitize dompurify (marked markdown)]
