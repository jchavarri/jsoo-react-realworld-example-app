open Promise
module SS = Set.Make (String)
module SI = Set.Make (Int)

type 'a asyncArticles = (Shape.articles, 'a App_error.t) Async_result.t

type 'a asyncTags = (string array, 'a App_error.t) Async_result.t

type asyncData = Shape.user option Async_data.t

type 'a asyncArticleEditor =
  (Shape.article_response * string * Shape.editor_error option, 'a App_error.t) Async_result.t

type 'a asyncArticle = (Shape.article_response, 'a App_error.t) Async_result.t

type 'a asyncComment = (Shape.comment array, 'a App_error.t) Async_result.t

type 'a asyncAuthor = (Shape.author, 'a App_error.t) Async_result.t

let useArticles ~(feedType : Shape.Feed_type.t) : 'a asyncArticles * (('a asyncArticles -> 'a asyncArticles) -> unit) =
  let data, setData = React.useState (fun () -> Async_result.init) in
  React.useEffect2
    (fun () ->
      setData (fun prev -> prev |> Async_result.toBusy);
      ( match feedType with
      | Tag (tag, limit, offset) -> Api.listArticles ~limit ~offset ?tag:(Some tag) ()
      | Global (limit, offset) -> Api.listArticles ~limit ~offset ()
      | Personal (limit, offset) -> Api.feedArticles ~limit ~offset ()
      )
      |> then_ ~fulfilled:(fun data ->
           setData (fun _prev ->
             match data with
             | Ok ok -> Async_result.completeOk ok
             | Error error -> Async_result.completeError error
           )
           |> resolve
         )
      |> ignore;
      None
    )
    (feedType, setData);
  data, setData

let useArticlesInProfile
  : viewMode:Shape.Profile.viewMode -> 'a asyncArticles * (('a asyncArticles -> 'a asyncArticles) -> unit)
  =
 fun ~viewMode ->
  let data, setData = React.useState (fun () -> Async_result.init) in
  React.useEffect2
    (fun () ->
      setData (fun prev -> prev |> Async_result.toBusy);
      ( match viewMode with
      | Author (author, limit, offset) -> Api.listArticles ~author ~limit ~offset ()
      | Favorited (favorited, limit, offset) -> Api.listArticles ~favorited ~limit ~offset ()
      )
      |> then_ ~fulfilled:(fun data ->
           setData (fun _prev ->
             match data with
             | Ok ok -> Async_result.completeOk ok
             | Error error -> Async_result.completeError error
           )
           |> resolve
         )
      |> ignore;
      None
    )
    (viewMode, setData);
  data, setData

let useTags : unit -> 'a asyncTags =
 fun () ->
  let data, setData = React.useState (fun () -> Async_result.init) in
  React.useEffect0 (fun () ->
    setData (fun prev -> prev |> Async_result.getOk |> Stdlib.Option.value ~default:[||] |> Async_result.reloadingOk);
    Api.tags ()
    |> then_ ~fulfilled:(fun data ->
         setData (fun _prev ->
           match data with
           | Ok ok -> ok |> Async_result.completeOk
           | Error error -> Async_result.completeError error
         )
         |> resolve
       )
    |> ignore;
    None
  );
  data

let useCurrentUser : unit -> asyncData * ((asyncData -> asyncData) -> unit) =
 fun () ->
  let data, setData = React.useState (fun () -> Async_data.init) in
  React.useEffect0 (fun () ->
    setData (fun prev -> prev |> Async_data.toBusy);
    Api.currentUser ()
    |> then_ ~fulfilled:(fun data ->
         setData (fun _prev ->
           match data with
           | Ok data' -> Some data' |> Async_data.complete
           | Error _error -> None |> Async_data.complete
         )
         |> resolve
       )
    |> catch ~rejected:(fun _error -> setData (fun _prev -> None |> Async_data.complete) |> resolve)
    |> ignore;
    None
  );
  data, setData

let useArticle ~(slug : string) : 'a asyncArticleEditor * (('a asyncArticleEditor -> 'a asyncArticleEditor) -> unit) =
  let data, setData = React.useState (fun () -> Async_result.init) in
  React.useEffect1
    (fun () ->
      setData Async_result.toBusy;
      Api.article ~action:(Read slug) ()
      |> then_ ~fulfilled:(fun data ->
           setData (fun _prev ->
             match data with
             | Ok (ok : Shape.article_response) -> Async_result.completeOk (ok, ok.tagList |> String.concat ",", None)
             | Error error -> Async_result.completeError error
           )
           |> resolve
         )
      |> ignore;
      None
    )
    [| slug |];
  data, setData

let useComments
  :  slug:string ->
  'a asyncComment * SI.t * (slug:string -> id:int -> unit) * (('a asyncComment -> 'a asyncComment) -> unit)
  =
 fun ~slug ->
  let data, setData = React.useState (fun () -> Async_result.init) in
  let busy, setBusy = React.useState (fun () -> SI.empty) in
  React.useEffect2
    (fun () ->
      setData Async_result.toBusy;
      setBusy (fun _prev -> SI.empty);
      Api.getComments ~slug ()
      |> then_ ~fulfilled:(fun data ->
           setData (fun _prev ->
             match data with
             | Ok ok -> Async_result.completeOk ok
             | Error error -> Async_result.completeError error
           )
           |> resolve
         )
      |> ignore;
      None
    )
    (slug, setData);
  let deleteComment ~slug ~id =
    setBusy (SI.add id);
    Api.deleteComment ~slug ~id ()
    |> then_ ~fulfilled:(fun resp ->
         setBusy (SI.remove id);
         ( match resp with
         | Ok (_slug, id) ->
           setData (fun prev ->
             prev
             |> Async_result.map (fun comments ->
                  comments
                  |> Stdlib.Array.to_list
                  |> Stdlib.List.filter (fun (comment : Shape.comment) -> comment.id <> id)
                  |> Stdlib.Array.of_list
                )
           )
         | Error _error -> ignore ()
         );
         ignore () |> resolve
       )
    |> ignore
  in
  data, busy, deleteComment, setData

let useFollow : article:'a asyncArticle -> user:Shape.user option -> (string * bool) Async_data.t * Link.onClickAction =
 fun ~article ~user ->
  let state, setState = React.useState (fun () -> Async_data.init) in
  let follow =
    match state with
    | Init ->
      article
      |> Async_result.getOk
      |> Stdlib.Option.map (fun (ok : Shape.article_response) ->
           Async_data.complete (ok.author.username, ok.author.following)
         )
      |> Stdlib.Option.value ~default:(Async_data.complete ("", false))
    | (Loading | Reloading _ | Complete _) as orig -> orig
  in
  let sendRequest () =
    let username =
      follow
      |> Async_data.getValue
      |> Stdlib.Option.map (fun (username, _following) -> username)
      |> Stdlib.Option.value ~default:""
    in
    let action =
      Stdlib.Option.bind (Async_data.getValue follow) (fun (_username, following) ->
        if following then Some (Api.Action.Unfollow username) else None
      )
      |> Stdlib.Option.value ~default:(Api.Action.Follow username)
    in
    setState (fun _prev -> follow |> Async_data.toBusy);
    Api.followUser ~action ()
    |> then_ ~fulfilled:(fun data ->
         setState (fun _prev ->
           match data with
           | Ok (ok : Shape.author) -> Async_data.complete (ok.username, ok.following)
           | Error _error -> Async_data.complete ("", false)
         )
         |> resolve
       )
    |> catch ~rejected:(fun _error -> setState (fun _prev -> Async_data.complete ("", false)) |> resolve)
    |> ignore
  in
  let onClick =
    match user with
    | Some _user -> Link.CustomFn (fun () -> sendRequest ())
    | None -> Location Link.register
  in
  follow, onClick

let useFollowInProfile
  : profile:'a asyncAuthor -> user:Shape.user option -> (string * bool) Async_data.t * Link.onClickAction
  =
 fun ~profile ~user ->
  let state, setState = React.useState (fun () -> Async_data.init) in
  let follow =
    match state with
    | Init ->
      profile
      |> Async_result.getOk
      |> Stdlib.Option.map (fun (ok : Shape.author) -> Async_data.complete (ok.username, ok.following))
      |> Stdlib.Option.value ~default:(Async_data.complete ("", false))
    | (Loading | Reloading _ | Complete _) as orig -> orig
  in
  let sendRequest () =
    let username =
      follow
      |> Async_data.getValue
      |> Stdlib.Option.map (fun (username, _following) -> username)
      |> Stdlib.Option.value ~default:""
    in
    let action =
      Stdlib.Option.bind (Async_data.getValue follow) (fun (_username, following) ->
        if following then Some (Api.Action.Unfollow username) else None
      )
      |> Stdlib.Option.value ~default:(Api.Action.Follow username)
    in
    setState (fun _prev -> follow |> Async_data.toBusy);
    Api.followUser ~action ()
    |> then_ ~fulfilled:(fun data ->
         setState (fun _prev ->
           match data with
           | Ok (ok : Shape.author) -> Async_data.complete (ok.username, ok.following)
           | Error _error -> Async_data.complete ("", false)
         )
         |> resolve
       )
    |> catch ~rejected:(fun _error -> setState (fun _prev -> Async_data.complete ("", false)) |> resolve)
    |> ignore
  in
  let onClick =
    match user with
    | Some _user -> Link.CustomFn (fun () -> sendRequest ())
    | None -> Location Link.register
  in
  follow, onClick

let useFavorite ~(article : 'a asyncArticle) ~(user : Shape.user option)
  : (bool * int * string) Async_data.t * Link.onClickAction
  =
  let state, setState = React.useState (fun () -> Async_data.init) in
  let favorite =
    match state with
    | Init ->
      article
      |> Async_result.getOk
      |> Stdlib.Option.map (fun (ok : Shape.article_response) ->
           Async_data.complete (ok.favorited, ok.favoritesCount, ok.slug)
         )
      |> Stdlib.Option.value ~default:(Async_data.complete (false, 0, ""))
    | (Loading | Reloading _ | Complete _) as orig -> orig
  in
  let sendRequest () =
    let favorited, _favoritesCount, slug =
      favorite |> Async_data.getValue |> Stdlib.Option.value ~default:(false, 0, "")
    in
    let action = if favorited then Api.Action.Unfavorite slug else Api.Action.Favorite slug in
    setState (fun _prev -> favorite |> Async_data.toBusy);
    Api.favoriteArticle ~action ()
    |> then_ ~fulfilled:(fun data ->
         setState (fun _prev ->
           match data with
           | Ok (ok : Shape.article_response) -> Async_data.complete (ok.favorited, ok.favoritesCount, ok.slug)
           | Error _error -> Async_data.complete (false, 0, "")
         )
         |> resolve
       )
    |> catch ~rejected:(fun _error -> setState (fun _prev -> Async_data.complete (false, 0, "")) |> resolve)
    |> ignore
  in
  let onClick =
    match user with
    | Some _user -> Link.CustomFn (fun () -> sendRequest ())
    | None -> Location Link.register
  in
  favorite, onClick

let useDeleteArticle : article:'a asyncArticle -> user:Shape.user option -> bool * Link.onClickAction =
 fun ~article ~user ->
  let state, setState = React.useState (fun () -> false) in
  let sendRequest () =
    let slug =
      article
      |> Async_result.getOk
      |> Stdlib.Option.map (fun (ok : Shape.article_response) -> ok.slug)
      |> Stdlib.Option.value ~default:""
    in
    setState (fun _prev -> true);
    Api.article ~action:(Delete slug) ()
    |> then_ ~fulfilled:(fun _data ->
         setState (fun _prev -> false);
         Link.push Link.home;
         ignore () |> resolve
       )
    |> catch ~rejected:(fun _error -> setState (fun _prev -> false) |> resolve)
    |> ignore
  in
  let onClick =
    match user, state with
    | Some _user, false ->
      Link.CustomFn
        (fun () ->
          let open Js_of_ocaml in
          if Dom_html.window##confirm (Js.string "Are you sure you want to delete this article?") |> Js.to_bool then
            sendRequest ()
          else ignore ()
        )
    | Some _, true | None, (true | false) -> Link.CustomFn ignore
  in
  state, onClick

let useToggleFavorite
  :  setArticles:(('a asyncArticles -> 'a asyncArticles) -> unit) -> user:Shape.user option ->
  SS.t * (action:Api.Action.favorite -> unit)
  =
 fun ~setArticles ~user ->
  let busy, setBusy = React.useState (fun () -> SS.empty) in
  let sendRequest ~action =
    let slug =
      match (action : Api.Action.favorite) with
      | Favorite slug | Unfavorite slug -> slug
    in
    setBusy (fun prev -> prev |> fun __x -> SS.add slug __x);
    Api.favoriteArticle ~action ()
    |> then_ ~fulfilled:(fun data ->
         setBusy (fun prev -> prev |> fun __x -> SS.remove slug __x);
         ( match data with
         | Ok _ ->
           setArticles (fun prev ->
             prev
             |> Async_result.map (fun (articles : Shape.articles) ->
                  {
                    articles with
                    articles =
                      articles.articles
                      |> Stdlib.Array.map (fun (article : Shape.article_response) ->
                           if article.slug = slug then
                             {
                               article with
                               favorited =
                                 ( match action with
                                 | Favorite _ -> true
                                 | Unfavorite _ -> false
                                 );
                               favoritesCount =
                                 ( match action with
                                 | Favorite _ -> article.favoritesCount + 1
                                 | Unfavorite _ -> article.favoritesCount - 1
                                 );
                             }
                           else article
                         );
                  }
                )
           )
         | Error _error -> ignore ()
         );
         ignore () |> resolve
       )
    |> catch ~rejected:(fun _error -> setBusy (fun prev -> prev |> fun __x -> SS.remove slug __x) |> resolve)
    |> ignore
  in
  let onToggle ~action =
    match user with
    | Some _ -> sendRequest ~action
    | None -> Link.push Link.register
  in
  busy, onToggle

let useProfile : username:string -> 'a asyncAuthor =
 fun ~username ->
  let data, setData = React.useState (fun () -> Async_result.init) in
  React.useEffect2
    (fun () ->
      setData (fun prev -> prev |> Async_result.toBusy);
      Api.getProfile ~username ()
      |> then_ ~fulfilled:(fun data ->
           setData (fun _prev ->
             match data with
             | Ok ok -> Async_result.completeOk ok
             | Error error -> Async_result.completeError error
           )
           |> resolve
         )
      |> ignore;
      None
    )
    (username, setData);
  data

let useViewMode : route:Shape.Profile.viewMode -> Shape.Profile.viewMode * (int -> unit) =
 fun ~route ->
  let viewMode, setViewMode = React.useState (fun () -> None) in
  let finalViewMode = viewMode |> Stdlib.Option.value ~default:route in
  React.useEffect2
    (fun () ->
      setViewMode (fun _prev -> None);
      None
    )
    (route, setViewMode);
  let changeOffset offset =
    setViewMode (fun _prev ->
      Some
        ( match finalViewMode with
        | Author (username, limit, _offset) -> Author (username, limit, offset)
        | Favorited (username, limit, _offset) -> Favorited (username, limit, offset)
        )
    )
  in
  finalViewMode, changeOffset
