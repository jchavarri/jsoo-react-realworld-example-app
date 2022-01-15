open Js_of_ocaml

module Action = struct
  type article =
    | Create of Shape.article_response
    | Read of string
    | Update of string * Shape.article_response
    | Delete of string

  type follow =
    | Follow of string
    | Unfollow of string

  type favorite =
    | Favorite of string
    | Unfavorite of string
end

module Headers = struct
  let add_jwt_token : unit -> (string * string) list =
   fun () ->
    Option.bind (Utils.getCookie "jwtToken") snd
    |> Option.map (fun token -> [ "Authorization", "Token " ^ token ])
    |> Option.value ~default:[]

  let add_content_type_as_json : unit -> (string * string) list =
   fun () -> [ "Content-Type", "application/json; charset=UTF-8" ]
end

let getErrorBodyJson : ('a Js.t, Fetch.Response.t Js.t) result -> ('a Js.t, 'b App_error.t) result Promise.t =
 fun x ->
  match x with
  | Ok _json as ok -> ok |> Promise.resolve
  | Error resp ->
    resp
    |> Fetch.Response.json
    |> Promise.then_ ~fulfilled:(fun json ->
         let status = Fetch.Response.status resp in
         let statusText = Fetch.Response.statusText resp in
         let bodyJson = `json json in
         Result.Error (App_error.fetch (status, statusText, bodyJson)) |> Promise.resolve
       )

let getErrorBodyText : ('a Js.t, Fetch.Response.t Js.t) result -> ('a Js.t, 'b App_error.t) result Promise.t =
 fun x ->
  let open Promise in
  match x with
  | Ok _json as ok -> ok |> resolve
  | Error resp ->
    let status = Fetch.Response.status resp in
    let statusText = Fetch.Response.statusText resp in
    let bodyText = `text "FIXME: show body text instead" in
    Error (App_error.fetch (status, statusText, bodyText)) |> resolve

let parseJsonIfOk : Fetch.Response.t Js.t -> ('a Js.t, Fetch.Response.t Js.t) result Promise.t =
 fun resp ->
  let open Promise in
  let open Fetch.Response in
  match ok resp with
  | true ->
    resp
    |> json
    |> then_ ~fulfilled:(fun json -> resolve (Ok json))
    |> catch ~rejected:(fun _error -> Error resp |> resolve)
  | false -> Error resp |> resolve

let article : action:Action.article -> unit -> (Shape.article_response, 'a App_error.t) result Promise.t =
 fun ~action () ->
  let body =
    match action with
    | Create article | Update (_, article) ->
      Some
        (Js_of_ocaml.Js._JSON##stringify
           (Shape.jsobject_of_article Shape.jsobject_of_create_article
              {
                article =
                  {
                    title = article.title;
                    description = article.description;
                    body = article.body;
                    tagList = article.tagList;
                  };
              }
           )
        |> Js_of_ocaml.Js.to_string
        |> Fetch.Body_init.make
        )
    | Read _ | Delete _ -> None
  in
  let method__ =
    match action with
    | Create _ -> Fetch.Post
    | Read _ -> Get
    | Update _ -> Put
    | Delete _ -> Delete
  in
  let headers =
    List.concat
      [
        ( match action with
        | Create _ | Update _ -> Headers.add_content_type_as_json ()
        | Read _ | Delete _ -> []
        );
        Headers.add_jwt_token ();
      ]
    |> Fetch.Headers_init.make
  in
  let slug =
    match action with
    | Create _ -> ""
    | Read slug | Update (slug, _) | Delete slug -> slug
  in
  let open Promise in
  Fetch.fetch_withInit (Endpoints.Articles.article ~slug ()) (Fetch.RequestInit.make ~method_:method__ ~headers ?body ())
  |> then_ ~fulfilled:parseJsonIfOk
  |> then_ ~fulfilled:getErrorBodyJson
  |> then_ ~fulfilled:(fun result ->
       Stdlib.Result.bind result (fun json ->
         json
         |> Shape.article_of_jsobject Shape.article_response_of_jsobject
         |> Stdlib.Result.map (fun (response : Shape.article_response Shape.article) -> response.article)
         |> App_error.decode
       )
       |> resolve
     )

let favoriteArticle : action:Action.favorite -> unit -> (Shape.article_response, 'a App_error.t) result Promise.t =
 fun ~action () ->
  let requestInit =
    Fetch.RequestInit.make
      ~method_:
        ( match action with
        | Favorite _slug -> Post
        | Unfavorite _slug -> Delete
        )
      ~headers:(Headers.add_jwt_token () |> Fetch.Headers_init.make)
      ()
  in
  let open Promise in
  Endpoints.Articles.favorite
    ~slug:
      ( match action with
      | Favorite slug -> slug
      | Unfavorite slug -> slug
      )
    ()
  |> (fun __x -> Fetch.fetch_withInit __x requestInit)
  |> then_ ~fulfilled:parseJsonIfOk
  |> then_ ~fulfilled:getErrorBodyText
  |> then_ ~fulfilled:(fun result ->
       Stdlib.Result.bind result (fun json ->
         json
         |> Shape.article_of_jsobject Shape.article_response_of_jsobject
         |> Stdlib.Result.map (fun (response : Shape.article_response Shape.article) -> response.article)
         |> App_error.decode
       )
       |> resolve
     )

let listArticles
  :  ?limit:int -> ?offset:int -> ?tag:string -> ?author:string -> ?favorited:string -> unit ->
  (Shape.articles, 'a App_error.t) result Promise.t
  =
 fun ?(limit = 10) ?(offset = 0) ?tag ?author ?favorited () ->
  let requestInit =
    let headers = Fetch.Headers_init.make (Headers.add_jwt_token ()) in
    Fetch.RequestInit.make ~headers ()
  in
  let open Promise in
  Endpoints.Articles.root ~limit ~offset ?tag ?author ?favorited ()
  |> (fun __x -> Fetch.fetch_withInit __x requestInit)
  |> then_ ~fulfilled:parseJsonIfOk
  |> then_ ~fulfilled:getErrorBodyText
  |> then_ ~fulfilled:(fun result ->
       Stdlib.Result.bind result (fun json -> json |> Shape.articles_of_jsobject |> App_error.decode) |> resolve
     )

let feedArticles : ?limit:int -> ?offset:int -> unit -> (Shape.articles, 'a App_error.t) result Promise.t =
 fun ?(limit = 10) ?(offset = 0) () ->
  let requestInit =
    let headers = Fetch.Headers_init.make (Headers.add_jwt_token ()) in
    Fetch.RequestInit.make ~headers ()
  in
  let open Promise in
  Endpoints.Articles.feed ~limit ~offset ()
  |> (fun __x -> Fetch.fetch_withInit __x requestInit)
  |> then_ ~fulfilled:parseJsonIfOk
  |> then_ ~fulfilled:getErrorBodyText
  |> then_ ~fulfilled:(fun result ->
       Stdlib.Result.bind result (fun json -> json |> Shape.articles_of_jsobject |> App_error.decode) |> resolve
     )

let tags : unit -> (string array, 'a App_error.t) result Promise.t =
 fun () ->
  let open Promise in
  Endpoints.tags
  |> Fetch.fetch
  |> then_ ~fulfilled:parseJsonIfOk
  |> then_ ~fulfilled:getErrorBodyText
  |> then_ ~fulfilled:(fun result ->
       Stdlib.Result.bind result (fun json ->
         json
         |> Shape.tags_of_jsobject
         |> Stdlib.Result.map (fun (response : Shape.tags) -> response.tags)
         |> App_error.decode
       )
       |> resolve
     )

let currentUser : unit -> (Shape.user, 'a App_error.t) result Promise.t =
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
         json
         |> Shape.user_response_of_jsobject
         |> Stdlib.Result.map (fun (response : Shape.user_response) -> response.user)
         |> App_error.decode
       )
       |> resolve
     )

let updateUser : user:Shape.user -> password:string -> unit -> (Shape.user, 'a App_error.t) result Promise.t =
 fun ~user ~password () ->
  let body =
    Js_of_ocaml.Js._JSON##stringify
      (Shape.jsobject_of_user_body Shape.jsobject_of_update_user
         {
           user =
             {
               email = user.email;
               username = user.username;
               bio = user.bio;
               image = user.image;
               password =
                 ( match password with
                 | "" -> None
                 | pw -> Some pw
                 );
             };
         }
      )
    |> Js_of_ocaml.Js.to_string
    |> Fetch.Body_init.make
  in
  let requestInit =
    Fetch.RequestInit.make ~method_:Put
      ~headers:(Fetch.Headers_init.make (List.concat [ Headers.add_jwt_token (); Headers.add_content_type_as_json () ]))
      ~body ()
  in
  let open Promise in
  Endpoints.user
  |> (fun __x -> Fetch.fetch_withInit __x requestInit)
  |> then_ ~fulfilled:parseJsonIfOk
  |> then_ ~fulfilled:getErrorBodyText
  |> then_ ~fulfilled:(fun result ->
       Stdlib.Result.bind result (fun json -> json |> Shape.user_of_jsobject |> App_error.decode) |> resolve
     )

let followUser : action:Action.follow -> unit -> (Shape.author, 'a App_error.t) result Promise.t =
 fun ~action () ->
  let requestInit =
    let headers = Fetch.Headers_init.make (Headers.add_jwt_token ()) in
    Fetch.RequestInit.make
      ~method_:
        ( match action with
        | Follow _username -> Post
        | Unfollow _username -> Delete
        )
      ~headers ()
  in
  let open Promise in
  Endpoints.Profiles.follow
    ~username:
      ( match action with
      | Follow username | Unfollow username -> username
      )
    ()
  |> (fun __x -> Fetch.fetch_withInit __x requestInit)
  |> then_ ~fulfilled:parseJsonIfOk
  |> then_ ~fulfilled:getErrorBodyText
  |> then_ ~fulfilled:(fun result ->
       Stdlib.Result.bind result (fun json ->
         json
         |> Shape.profile_of_jsobject
         |> Stdlib.Result.map (fun (response : Shape.profile) -> response.author)
         |> App_error.decode
       )
       |> resolve
     )

let getComments : slug:string -> unit -> (Shape.comment array, 'a App_error.t) result Promise.t =
 fun ~slug () ->
  let requestInit =
    let headers = Fetch.Headers_init.make (Headers.add_jwt_token ()) in
    Fetch.RequestInit.make ~headers ()
  in
  let open Promise in
  Endpoints.Articles.comments ~slug ()
  |> (fun __x -> Fetch.fetch_withInit __x requestInit)
  |> then_ ~fulfilled:parseJsonIfOk
  |> then_ ~fulfilled:getErrorBodyText
  |> then_ ~fulfilled:(fun result ->
       Stdlib.Result.bind result (fun json ->
         json
         |> Shape.comments_of_jsobject
         |> Stdlib.Result.map (fun (response : Shape.comments) -> response.comments)
         |> App_error.decode
       )
       |> resolve
     )

let deleteComment : slug:string -> id:int -> unit -> (string * int, 'a App_error.t) result Promise.t =
 fun ~slug ~id () ->
  let requestInit =
    Fetch.RequestInit.make ~method_:Delete ~headers:(Fetch.Headers_init.make (Headers.add_jwt_token ())) ()
  in
  let open Promise in
  Endpoints.Articles.comment ~slug ~id ()
  |> (fun __x -> Fetch.fetch_withInit __x requestInit)
  |> then_ ~fulfilled:parseJsonIfOk
  |> then_ ~fulfilled:getErrorBodyText
  |> then_ ~fulfilled:(fun result -> Stdlib.Result.bind result (fun _json -> Ok (slug, id)) |> resolve)

let addComment : slug:string -> body:string -> unit -> (Shape.comment, 'a App_error.t) result Promise.t =
 fun ~slug ~body () ->
  let body =
    Js_of_ocaml.Js._JSON##stringify (Shape.jsobject_of_add_comment_payload { comment = { body } })
    |> Js_of_ocaml.Js.to_string
    |> Fetch.Body_init.make
  in
  let requestInit =
    Fetch.RequestInit.make ~method_:Post
      ~headers:(Fetch.Headers_init.make (List.concat [ Headers.add_jwt_token (); Headers.add_content_type_as_json () ]))
      ~body ()
  in
  let open Promise in
  Endpoints.Articles.comments ~slug ()
  |> (fun __x -> Fetch.fetch_withInit __x requestInit)
  |> then_ ~fulfilled:parseJsonIfOk
  |> then_ ~fulfilled:getErrorBodyText
  |> then_ ~fulfilled:(fun result ->
       Stdlib.Result.bind result (fun json ->
         json
         |> Shape.add_comment_response_of_jsobject
         |> Stdlib.Result.map (fun (response : Shape.add_comment_response) -> response.comment)
         |> App_error.decode
       )
       |> resolve
     )

let getProfile : username:string -> unit -> (Shape.author, 'a App_error.t) result Promise.t =
 fun ~username () ->
  let requestInit =
    let headers = Fetch.Headers_init.make (Headers.add_jwt_token ()) in
    Fetch.RequestInit.make ~headers ()
  in
  let open Promise in
  Endpoints.Profiles.profile ~username ()
  |> (fun __x -> Fetch.fetch_withInit __x requestInit)
  |> then_ ~fulfilled:parseJsonIfOk
  |> then_ ~fulfilled:getErrorBodyText
  |> then_ ~fulfilled:(fun result ->
       Stdlib.Result.bind result (fun json ->
         json
         |> Shape.profile_of_jsobject
         |> Stdlib.Result.map (fun (response : Shape.profile) -> response.author)
         |> App_error.decode
       )
       |> resolve
     )

let login ~(email : string) ~(password : string) () : (Shape.user, 'a App_error.t) result Promise.t =
  let body =
    Js_of_ocaml.Js._JSON##stringify (Shape.jsobject_of_login_body { user = { email; password } })
    |> Js_of_ocaml.Js.to_string
    |> Fetch.Body_init.make
  in
  let requestInit =
    Fetch.RequestInit.make ~method_:Post
      ~headers:(Fetch.Headers_init.make (Headers.add_content_type_as_json ()))
      ~body ()
  in
  let open Promise in
  Endpoints.Users.login
  |> (fun __x -> Fetch.fetch_withInit __x requestInit)
  |> then_ ~fulfilled:parseJsonIfOk
  |> then_ ~fulfilled:getErrorBodyText
  |> then_ ~fulfilled:(fun result ->
       Stdlib.Result.bind result (fun json ->
         json
         |> Shape.user_response_of_jsobject
         |> Stdlib.Result.map (fun (response : Shape.user_response) -> response.user)
         |> App_error.decode
       )
       |> resolve
     )

let register
  : username:string -> email:string -> password:string -> unit -> (Shape.user, 'a App_error.t) result Promise.t
  =
 fun ~username ~email ~password () ->
  let body =
    Js_of_ocaml.Js._JSON##stringify
      (Shape.jsobject_of_user_body Shape.jsobject_of_register { user = { email; username; password } })
    |> Js_of_ocaml.Js.to_string
    |> Fetch.Body_init.make
  in
  let requestInit =
    Fetch.RequestInit.make ~method_:Post
      ~headers:(Fetch.Headers_init.make (Headers.add_content_type_as_json ()))
      ~body ()
  in
  let open Promise in
  Endpoints.Users.root
  |> (fun __x -> Fetch.fetch_withInit __x requestInit)
  |> then_ ~fulfilled:parseJsonIfOk
  |> then_ ~fulfilled:getErrorBodyJson
  |> then_ ~fulfilled:(fun result ->
       Stdlib.Result.bind result (fun json -> json |> Shape.user_of_jsobject |> App_error.decode) |> resolve
     )
