open Js_of_ocaml

module Action = struct
  type article =
    | Create of Shape_t.article
    | Read of string
    | Update of string * Shape_t.article
    | Delete of string

  type follow = Follow of string | Unfollow of string

  type favorite = Favorite of string | Unfavorite of string
end

module Headers = struct
  let add_jwt_token : unit -> (string * string) list =
   fun () ->
    Option.bind (Utils.getCookie "jwtToken") snd
    |> Option.map (fun token -> [("Authorization", "Token " ^ token)])
    |> Option.value ~default:[]

  let addContentTypeAsJson : unit -> (string * string) array =
   fun () -> [|("Content-Type", "application/json; charset=UTF-8")|]
end

(*
    let getErrorBodyJson :
        (Js.Json.t, Response.t) result -> (Js.Json.t, AppError.t) result Promise.t =
     fun x ->
      match x with
      | Ok _json as ok ->
          ok |> resolve
      | Error resp ->
          resp |> Response.json
          |> then_ (fun json ->
                 let status = Response.status resp in
                 let statusText = Response.statusText resp in
                 let bodyJson = `json json in
                 AppError.fetch (status, statusText, bodyJson)
                 |> Belt.Result.Error |> resolve )
*)
let getErrorBodyText :
       ('a Js.t, Fetch.Response.t Js.t) result
    -> ('a Js.t, App_error.t) result Promise.t =
 fun x ->
  let open Promise in
  match x with
  | Ok _json as ok ->
      ok |> resolve
  | Error resp ->
      let status = Fetch.Response.status resp in
      let statusText = Fetch.Response.statusText resp in
      let bodyText = `text "FIXME: show body text instead" in
      Error (App_error.fetch (status, statusText, bodyText)) |> resolve

let parseJsonIfOk :
    Fetch.Response.t Js.t -> ('a Js.t, Fetch.Response.t Js.t) result Promise.t =
 fun resp ->
  let open Promise in
  let open Fetch.Response in
  match ok resp with
  | true ->
      resp |> json
      |> then_ ~fulfilled:(fun json -> resolve (Ok json))
      |> catch ~rejected:(fun _error -> Error resp |> resolve)
  | false ->
      Error resp |> resolve

(*
   let article :
          action:Action.article
       -> unit
       -> (Shape_t.article, AppError.t) result Promise.t =
    fun ~action () ->
     let body =
       match action with
       | Create article | Update (_, article) ->
           let article =
             [ ("title", Js.Json.string article.title)
             ; ("description", Js.Json.string article.description)
             ; ("body", Js.Json.string article.body)
             ; ("tagList", Js.Json.stringArray article.tagList) ]
             |> Js.Dict.fromList |> Js.Json.object_
           in
           [("article", article)] |> Js.Dict.fromList |> Js.Json.object_
           |> Js.Json.stringify |> BodyInit.make |> Some
       | Read _ | Delete _ ->
           None
     in
     let method__ =
       match action with
       | Create _ ->
           Post
       | Read _ ->
           Get
       | Update _ ->
           Put
       | Delete _ ->
           Delete
     in
     let headers =
       ( match action with
       | Create _ | Update _ ->
           Headers.addContentTypeAsJson ()
       | Read _ | Delete _ ->
           [||] )
       |> Belt.Array.concat (Headers.addJwtToken ())
       |> HeadersInit.makeWithArray
     in
     let slug =
       match action with
       | Create _ ->
           ""
       | Read slug | Update (slug, _) | Delete slug ->
           slug
     in
     fetchWithInit
       (Endpoints.Articles.article ~slug ())
       (RequestInit.make ~method_:method__ ~headers ?body ())
     |> then_ parseJsonIfOk |> then_ getErrorBodyJson
     |> then_ (fun result ->
            result
            |> Belt.Result.flatMap (fun json ->
                   try
                     json |> Js.Json.decodeObject |> Belt.Option.getExn
                     |> Js.Dict.get "article" |> Belt.Option.getExn
                     |> Shape.Article.decode |> AppError.decode
                   with _ ->
                     AppError.decode (Error "API.article: failed to decode json") )
            |> resolve )

   let favoriteArticle :
          action:Action.favorite
       -> unit
       -> (Shape_t.article, AppError.t) result Promise.t =
    fun ~action () ->
     let requestInit =
       RequestInit.make
         ~method_:
           (match action with Favorite _slug -> Post | Unfavorite _slug -> Delete)
         ~headers:(Headers.addJwtToken () |> HeadersInit.makeWithArray)
         ()
     in
     Endpoints.Articles.favorite
       ~slug:(match action with Favorite slug -> slug | Unfavorite slug -> slug)
       ()
     |> (fun __x -> fetchWithInit __x requestInit)
     |> then_ parseJsonIfOk |> then_ getErrorBodyText
     |> then_ (fun result ->
            result
            |> Belt.Result.flatMap (fun json ->
                   try
                     json |> Js.Json.decodeObject |> Belt.Option.getExn
                     |> Js.Dict.get "article" |> Belt.Option.getExn
                     |> Shape.Article.decode |> AppError.decode
                   with _ ->
                     AppError.decode
                       (Error "API.favoriteArticle: failed to decode json") )
            |> resolve )

   let listArticles :
          ?limit:int
       -> ?offset:int
       -> ?tag:string
       -> ?author:string
       -> ?favorited:string
       -> unit
       -> (Shape.Articles.t, AppError.t) result Promise.t =
    fun ?(limit = 10) ?(offset = 0) ?tag ?author ?favorited () ->
     let requestInit =
       RequestInit.make
         ~headers:(Headers.addJwtToken () |> HeadersInit.makeWithArray)
         ()
     in
     Endpoints.Articles.root ~limit ~offset ?tag ?author ?favorited ()
     |> (fun __x -> fetchWithInit __x requestInit)
     |> then_ parseJsonIfOk |> then_ getErrorBodyText
     |> then_ (fun result ->
            result
            |> Belt.Result.flatMap (fun json ->
                   json |> Shape.Articles.decode |> AppError.decode )
            |> resolve )

   let feedArticles :
          ?limit:int
       -> ?offset:int
       -> unit
       -> (Shape.Articles.t, AppError.t) result Promise.t =
    fun ?(limit = 10) ?(offset = 0) () ->
     let requestInit =
       RequestInit.make
         ~headers:(Headers.addJwtToken () |> HeadersInit.makeWithArray)
         ()
     in
     Endpoints.Articles.feed ~limit ~offset ()
     |> (fun __x -> fetchWithInit __x requestInit)
     |> then_ parseJsonIfOk |> then_ getErrorBodyText
     |> then_ (fun result ->
            result
            |> Belt.Result.flatMap (fun json ->
                   json |> Shape.Articles.decode |> AppError.decode )
            |> resolve )

   let tags : unit -> (Shape.Tags.t, AppError.t) result Promise.t =
    fun () ->
     Endpoints.tags |> fetch |> then_ parseJsonIfOk |> then_ getErrorBodyText
     |> then_ (fun result ->
            result
            |> Belt.Result.flatMap (fun json ->
                   json |> Shape.Tags.decode |> AppError.decode )
            |> resolve )
*)

let currentUser : unit -> (Shape.user, App_error.t) result Promise.t =
 fun () ->
  let requestInit =
    let headers = Fetch.Headers_init.make (Headers.add_jwt_token ()) in
    Fetch.RequestInit.make ~headers ()
  in
  let open Promise in
  Endpoints.user
  |> (fun __x -> Fetch.fetch_withInit __x requestInit)
  |> then_ ~fulfilled:parseJsonIfOk
  |> then_ ~fulfilled:getErrorBodyText
  |> then_ ~fulfilled:(fun result ->
         Stdlib.Result.bind result (fun json ->
             json |> Shape.user_of_jsobject |> App_error.decode )
         |> resolve )
(*
   let updateUser :
          user:Shape.User.t
       -> password:string
       -> unit
       -> (Shape.User.t, AppError.t) result Promise.t =
    fun ~user ~password () ->
     let user =
       [ [("email", Js.Json.string user.email)]
       ; [("bio", Js.Json.string (user.bio |> Belt.Option.getWithDefault ""))]
       ; [("image", Js.Json.string (user.image |> Belt.Option.getWithDefault ""))]
       ; [("username", Js.Json.string user.username)]
       ; (if password = "" then [] else [("password", Js.Json.string password)]) ]
       |> List.flatten |> Js.Dict.fromList |> Js.Json.object_
     in
     let body =
       [("user", user)] |> Js.Dict.fromList |> Js.Json.object_ |> Js.Json.stringify
       |> BodyInit.make
     in
     let requestInit =
       RequestInit.make ~method_:Put
         ~headers:
           ( Headers.addJwtToken ()
           |> Belt.Array.concat (Headers.addContentTypeAsJson ())
           |> HeadersInit.makeWithArray )
         ~body ()
     in
     Endpoints.user
     |> (fun __x -> fetchWithInit __x requestInit)
     |> then_ parseJsonIfOk |> then_ getErrorBodyJson
     |> then_ (fun result ->
            result
            |> Belt.Result.flatMap (fun json ->
                   json |> Shape.User.decode |> AppError.decode )
            |> resolve )

   let followUser :
          action:Action.follow
       -> unit
       -> (Shape.Author.t, AppError.t) result Promise.t =
    fun ~action () ->
     let requestInit =
       RequestInit.make
         ~method_:
           ( match action with
           | Follow _username ->
               Post
           | Unfollow _username ->
               Delete )
         ~headers:(Headers.addJwtToken () |> HeadersInit.makeWithArray)
         ()
     in
     Endpoints.Profiles.follow
       ~username:
         (match action with Follow username | Unfollow username -> username)
       ()
     |> (fun __x -> fetchWithInit __x requestInit)
     |> then_ parseJsonIfOk |> then_ getErrorBodyText
     |> then_ (fun result ->
            result
            |> Belt.Result.flatMap (fun json ->
                   try
                     json |> Js.Json.decodeObject |> Belt.Option.getExn
                     |> Js.Dict.get "profile" |> Belt.Option.getExn
                     |> Shape.Author.decode |> AppError.decode
                   with _ ->
                     AppError.decode
                       (Belt.Result.Error "API.followUser: failed to decode json") )
            |> resolve )

   let getComments :
       slug:string -> unit -> (Shape.Comment.t array, AppError.t) result Promise.t
       =
    fun ~slug () ->
     let requestInit =
       RequestInit.make
         ~headers:(Headers.addJwtToken () |> HeadersInit.makeWithArray)
         ()
     in
     Endpoints.Articles.comments ~slug ()
     |> (fun __x -> fetchWithInit __x requestInit)
     |> then_ parseJsonIfOk |> then_ getErrorBodyText
     |> then_ (fun result ->
            result
            |> Belt.Result.flatMap (fun json ->
                   json |> Shape.Comment.decode |> AppError.decode )
            |> resolve )

   let deleteComment :
       slug:string -> id:int -> unit -> (string * int, AppError.t) result Promise.t
       =
    fun ~slug ~id () ->
     let requestInit =
       RequestInit.make ~method_:Delete
         ~headers:(Headers.addJwtToken () |> HeadersInit.makeWithArray)
         ()
     in
     Endpoints.Articles.comment ~slug ~id ()
     |> (fun __x -> fetchWithInit __x requestInit)
     |> then_ parseJsonIfOk |> then_ getErrorBodyText
     |> then_ (fun result ->
            result
            |> Belt.Result.flatMap (fun _json -> Belt.Result.Ok (slug, id))
            |> resolve )

   let addComment :
          slug:string
       -> body:string
       -> unit
       -> (Shape.Comment.t, AppError.t) result Promise.t =
    fun ~slug ~body () ->
     let comment =
       [("body", Js.Json.string body)] |> Js.Dict.fromList |> Js.Json.object_
     in
     let body =
       [("comment", comment)] |> Js.Dict.fromList |> Js.Json.object_
       |> Js.Json.stringify |> BodyInit.make
     in
     let requestInit =
       RequestInit.make ~method_:Post
         ~headers:
           ( Headers.addJwtToken ()
           |> Belt.Array.concat (Headers.addContentTypeAsJson ())
           |> HeadersInit.makeWithArray )
         ~body ()
     in
     Endpoints.Articles.comments ~slug ()
     |> (fun __x -> fetchWithInit __x requestInit)
     |> then_ parseJsonIfOk |> then_ getErrorBodyText
     |> then_ (fun result ->
            result
            |> Belt.Result.flatMap (fun json ->
                   try
                     json |> Js.Json.decodeObject |> Belt.Option.getExn
                     |> Js.Dict.get "comment" |> Belt.Option.getExn
                     |> Shape.Comment.decodeComment |> AppError.decode
                   with _ ->
                     AppError.decode
                       (Belt.Result.Error "API.addComment: failed to decode json") )
            |> resolve )

   let getProfile :
       username:string -> unit -> (Shape.Author.t, AppError.t) result Promise.t =
    fun ~username () ->
     let requestInit =
       RequestInit.make
         ~headers:(Headers.addJwtToken () |> HeadersInit.makeWithArray)
         ()
     in
     Endpoints.Profiles.profile ~username ()
     |> (fun __x -> fetchWithInit __x requestInit)
     |> then_ parseJsonIfOk |> then_ getErrorBodyText
     |> then_ (fun result ->
            result
            |> Belt.Result.flatMap (fun json ->
                   try
                     json |> Js.Json.decodeObject |> Belt.Option.getExn
                     |> Js.Dict.get "profile" |> Belt.Option.getExn
                     |> Shape.Author.decode |> AppError.decode
                   with _ ->
                     AppError.decode
                       (Belt.Result.Error "API.getProfile: failed to decode json") )
            |> resolve )

   let login ~(email : string) ~(password : string) () :
       (Shape.User.t, AppError.t) result Promise.t =
     let user =
       [("email", Js.Json.string email); ("password", Js.Json.string password)]
       |> Js.Dict.fromList |> Js.Json.object_
     in
     let body =
       [("user", user)] |> Js.Dict.fromList |> Js.Json.object_ |> Js.Json.stringify
       |> BodyInit.make
     in
     let requestInit =
       RequestInit.make ~method_:Post
         ~headers:(Headers.addContentTypeAsJson () |> HeadersInit.makeWithArray)
         ~body ()
     in
     Endpoints.Users.login
     |> (fun __x -> fetchWithInit __x requestInit)
     |> then_ parseJsonIfOk |> then_ getErrorBodyJson
     |> then_ (fun result ->
            result
            |> Belt.Result.flatMap (fun json ->
                   json |> Shape.User.decode |> AppError.decode )
            |> resolve )

   let register :
          username:string
       -> email:string
       -> password:string
       -> unit
       -> (Shape.User.t, AppError.t) result Promise.t =
    fun ~username ~email ~password () ->
     let user =
       [ ("email", Js.Json.string email)
       ; ("password", Js.Json.string password)
       ; ("username", Js.Json.string username) ]
       |> Js.Dict.fromList |> Js.Json.object_
     in
     let body =
       [("user", user)] |> Js.Dict.fromList |> Js.Json.object_ |> Js.Json.stringify
       |> BodyInit.make
     in
     let requestInit =
       RequestInit.make ~method_:Post
         ~headers:(Headers.addContentTypeAsJson () |> HeadersInit.makeWithArray)
         ~body ()
     in
     Endpoints.Users.root
     |> (fun __x -> fetchWithInit __x requestInit)
     |> then_ parseJsonIfOk |> then_ getErrorBodyJson
     |> then_ (fun result ->
            result
            |> Belt.Result.flatMap (fun json ->
                   json |> Shape.User.decode |> AppError.decode )
            |> resolve ) *)
