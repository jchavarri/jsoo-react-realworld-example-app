let backend : string =
  let open Js_of_ocaml.Js in
  to_string Unsafe.global##.app##.backend

module Articles = struct
  let root : ?limit:int -> ?offset:int -> ?tag:string -> ?author:string -> ?favorited:string -> unit -> string =
   fun ?(limit = 10) ?(offset = 0) ?tag ?author ?favorited () ->
    let limit = string_of_int limit in
    let offset = string_of_int offset in
    let tag = tag |> Option.map (fun tag' -> "&tag=" ^ tag') |> Option.value ~default:"" in
    let author = author |> Option.map (fun author' -> "&author=" ^ author') |> Option.value ~default:"" in
    let favorited =
      favorited |> Option.map (fun favorited' -> "&favorited=" ^ favorited') |> Option.value ~default:""
    in
    backend ^ "/api/articles?limit=" ^ limit ^ "&offset=" ^ offset ^ tag ^ author ^ favorited

  let article : slug:string -> unit -> string = fun ~slug () -> backend ^ "/api/articles/" ^ slug

  let favorite : slug:string -> unit -> string = fun ~slug () -> backend ^ "/api/articles/" ^ slug ^ "/favorite"

  let feed : ?limit:int -> ?offset:int -> unit -> string =
   fun ?(limit = 10) ?(offset = 0) () ->
    let limit = string_of_int limit in
    let offset = string_of_int offset in
    backend ^ "/api/articles/feed?limit=" ^ limit ^ "&offset=" ^ offset

  let comments : slug:string -> unit -> string =
   fun ~(slug : string) () -> backend ^ "/api/articles/" ^ slug ^ "/comments"

  let comment : slug:string -> id:int -> unit -> string =
   fun ~slug ~id () ->
    let id = string_of_int id in
    backend ^ "/api/articles/" ^ slug ^ "/comments/" ^ id
end

module Profiles = struct
  let profile : username:string -> unit -> string = fun ~username () -> backend ^ "/api/profiles/" ^ username

  let follow : username:string -> unit -> string = fun ~username () -> backend ^ "/api/profiles/" ^ username ^ "/follow"
end

module Users = struct
  let root = backend ^ "/api/users"

  let login = backend ^ "/api/users/login"
end

let tags = backend ^ "/api/tags"

let user = backend ^ "/api/user"
