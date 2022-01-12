[@@@react.dom]

module Article_preview = Home_article_preview
module Popular_tags = Home_popular_tags

let useFeedType ~(user : Shape.user option) =
  React.useState (fun () ->
    match user with
    | None -> Shape.Feed_type.Global (10, 0)
    | Some _ -> Shape.Feed_type.Personal (10, 0)
  )

let%component make ~user =
  let feedType, setFeedType = useFeedType ~user in
  let articles, setArticles = Hook.useArticles ~feedType in
  let tags = Hook.useTags () in
  let toggleFavoriteBusy, onToggleFavorite = Hook.useToggleFavorite ~setArticles ~user in
  div ~className:"home-page"
    ~children:
      [
        div ~className:"banner"
          ~children:
            [
              div ~className:"container"
                ~children:
                  [
                    h1 ~className:"logo-font" ~children:[ "conduit" |> React.string ] ();
                    p ~children:[ "A place to share your knowledge." |> React.string ] ();
                  ]
                ();
            ]
          ();
        div ~className:"container page"
          ~children:
            [
              div ~className:"row"
                ~children:
                  [
                    div ~className:"col-md-9"
                      ~children:
                        [
                          With_test_id.make ~id:"feed-toggle"
                            ~children:
                              (div ~className:"feed-toggle"
                                 ~children:
                                   [
                                     ul ~className:"nav nav-pills outline-active"
                                       ~children:
                                         [
                                           Security.AuthenticatedOnly.make ~user
                                             ~children:
                                               [
                                                 li ~className:"nav-item"
                                                   ~children:
                                                     [
                                                       a
                                                         ~className:
                                                           ( match feedType with
                                                           | Tag _ | Global _ -> "nav-link"
                                                           | Personal _ -> "nav-link active"
                                                           )
                                                         ~href:"#your_feed"
                                                         ~onClick:(fun event ->
                                                           if Utils.isMouseRightClick event then (
                                                             event |> React.Event.Mouse.preventDefault;
                                                             setFeedType (fun _ -> Personal (10, 0))
                                                           )
                                                         )
                                                         ~children:[ "Your Feed" |> React.string ]
                                                         ();
                                                     ]
                                                   ();
                                               ]
                                             ();
                                           li ~className:"nav-item"
                                             ~children:
                                               [
                                                 a
                                                   ~className:
                                                     ( match feedType with
                                                     | Global _ -> "nav-link active"
                                                     | Tag _ | Personal _ -> "nav-link"
                                                     )
                                                   ~href:"#global"
                                                   ~onClick:(fun event ->
                                                     if Utils.isMouseRightClick event then (
                                                       event |> React.Event.Mouse.preventDefault;
                                                       setFeedType (fun _ -> Global (10, 0))
                                                     )
                                                   )
                                                   ~children:[ "Global Feed" |> React.string ]
                                                   ();
                                               ]
                                             ();
                                           ( match feedType with
                                           | Tag (tag, _, _) ->
                                             li ~className:"nav-item"
                                               ~children:
                                                 [
                                                   a ~className:"nav-link active" ~href:"#"
                                                     ~onClick:(fun event -> event |> React.Event.Mouse.preventDefault)
                                                     ~children:
                                                       [
                                                         i ~className:"ion-pound" ~children:[] ();
                                                         " " |> React.string;
                                                         tag |> React.string;
                                                       ]
                                                     ();
                                                 ]
                                               ()
                                           | Global _ | Personal _ -> React.null
                                           );
                                           ( if articles |> Async_result.isBusy then
                                             li ~className:"nav-item" ~children:[ Spinner.make () ] ()
                                           else React.null
                                           );
                                         ]
                                       ();
                                   ]
                              )
                            ();
                          ( match articles with
                          | Init -> React.null
                          | Loading -> React.null
                          | Reloading (Ok { articles; articlesCount = _ })
                          | Complete (Ok { articles; articlesCount = _ }) ->
                            articles
                            |> Array.map (fun item ->
                                 Article_preview.make ~key:item.slug ~data:item ~onToggleFavorite
                                   ~isFavoriteBusy:(toggleFavoriteBusy |> fun __x -> Hook.SS.mem item.slug __x)
                                   ()
                               )
                            |> Array.to_list
                            |> React.list
                          | Reloading (Error _error) | Complete (Error _error) -> React.string "ERROR"
                          );
                          ( match feedType with
                          | Tag (_, limit, offset) | Global (limit, offset) | Personal (limit, offset) ->
                            let total =
                              match articles with
                              | Init | Loading | Reloading (Error _) | Complete (Error _) -> 0
                              | Reloading (Ok { articlesCount; articles = _ })
                              | Complete (Ok { articlesCount; articles = _ }) ->
                                articlesCount
                            in
                            Pagination.make ~limit ~offset ~total
                              ~onClick:(fun offset ->
                                setFeedType (fun x ->
                                  match x with
                                  | Tag (tag, limit, _) -> Tag (tag, limit, offset)
                                  | Global (limit, _) -> Global (limit, offset)
                                  | Personal (limit, _) -> Personal (limit, offset)
                                )
                              )
                              ()
                          );
                        ]
                      ();
                    div ~className:"col-md-3"
                      ~children:
                        [
                          div ~className:"sidebar"
                            ~children:
                              [
                                Popular_tags.make ~data:tags
                                  ~onClick:(fun tag ->
                                    setFeedType (fun x ->
                                      match x with
                                      | Tag (_, limit, offset) -> Tag (tag, limit, offset)
                                      | Global _ | Personal _ -> Tag (tag, 10, 0)
                                    )
                                  )
                                  ();
                              ]
                            ();
                        ]
                      ();
                  ]
                ();
            ]
          ();
      ]
    ()
