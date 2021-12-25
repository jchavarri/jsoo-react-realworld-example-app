open Js_of_ocaml

type cookiePair = string * string option

let secondInMs = 1000.

let minuteInMs = 60. *. secondInMs

let hourInMs = 60. *. minuteInMs

let dayInMs = 24. *. hourInMs

let monthInMs = 30. *. dayInMs

let parseCookies : unit -> cookiePair list =
 fun () ->
  Dom_html.document##.cookie
  |> Js.to_string
  |> String.split_on_char ';'
  |> List.fold_left
       (fun acc str ->
         let pair = str |> String.split_on_char '=' |> List.map String.trim in
         let key = List.nth pair 0 in
         let value = List.nth_opt pair 1 in
         (key, value) :: acc
       )
       []

let getCookie (name : string) : cookiePair option =
  parseCookies ()
  |> List.find_opt (fun pair ->
       let key = fst pair in
       key = name
     )

let setCookieRaw : key:string -> ?value:string -> expires:string -> ?path:string -> unit -> unit =
 fun ~key ?value ~expires ?path () ->
  let htmlDocument = Dom_html.document in
  let value = value |> Option.value ~default:"" in
  let expires =
    match expires with
    | "" -> ""
    | _ -> "expires=" ^ expires ^ ";"
  in
  let path =
    Option.bind path (fun path -> if path = "" then None else Some path)
    |> Option.map (fun path -> " path=" ^ path ^ ";")
    |> Option.value ~default:""
  in
  let cookie = key ^ "=" ^ value ^ ";" ^ expires ^ path in
  Js_of_ocaml.Js.debugger ();
  htmlDocument##.cookie := Js.string cookie

let setCookie : string -> string option -> unit =
 fun key value ->
  let expires = new%js Js.date_now in
  let _ = expires##setTime (expires##getTime +. monthInMs) in
  setCookieRaw ~key ?value ~expires:(Js.to_string expires##toUTCString) ~path:"/" ()

let deleteCookie : string -> unit = fun key -> setCookieRaw ~key ~expires:"Thu, 01 Jan 1970 00:00:01 GMT" ()

let isMouseRightClick event =
  (not (React.Event.Mouse.defaultPrevented event))
  && React.Event.Mouse.button event = 0
  && (not (React.Event.Mouse.altKey event))
  && (not (React.Event.Mouse.ctrlKey event))
  && (not (React.Event.Mouse.metaKey event))
  && not (React.Event.Mouse.shiftKey event)

let formatDate : Js.date Js.t -> string =
 fun date ->
  let yyyy = string_of_int date##getFullYear in
  let mm = string_of_int date##getMonth in
  let dd = string_of_int date##getDate in
  yyyy ^ "/" ^ mm ^ "/" ^ dd

module Timestamp_str = struct
  type t = Js.date Js.t

  let of_jsobject x = Ok (new%js Js.date_fromTimeValue (Js.date##parse x))

  let jsobject_of (x : Js.date Js.t) = x##toISOString
end
