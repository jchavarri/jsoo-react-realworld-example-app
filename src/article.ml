[@@@react.dom]

let%component make ~(slug : string) ~user =
  let articleAndTagList, _setArticle = Hook.useArticle ~slug in
  let article = articleAndTagList |> Async_result.map (fun (article, _tagList, _editor) -> article) in
  let comments, busyComments, deleteComment, setComments = Hook.useComments ~slug in
  let follow, onFollowClick = Hook.useFollow ~article ~user in
  let favorite, onFavoriteClick = Hook.useFavorite ~article ~user in
  let isDeleteBusy, onDeleteClick = Hook.useDeleteArticle ~article ~user in
  let isAuthor =
    match user, article with
    | Some u, (Reloading (Ok a) | Complete (Ok a)) -> u.username = a.author.username
    | Some _, (Init | Loading | Reloading (Error _) | Complete (Error _))
    | None, (Init | Loading | Reloading _ | Complete _) ->
      false
  in
  div ~className:"article-page"
    ~children:
      [
        div ~className:"banner"
          ~children:
            [
              div ~className:"container"
                ~children:
                  [
                    h1
                      ~children:
                        [
                          article
                          |> Async_result.getOk
                          |> Option.map (fun (ok : Shape.article_response) -> ok.title)
                          |> Option.map (fun title -> title |> React.string)
                          |> Option.value ~default:React.null;
                        ]
                      ();
                    div ~className:"article-meta"
                      ~children:
                        [
                          Article_author_avatar.make ~article ();
                          div ~className:"info"
                            ~children:
                              [
                                Article_author_name.make ~article ();
                                span ~className:"date" ~children:[ Article_date.make ~article () ] ();
                              ]
                            ();
                          ( if isAuthor then Article_edit_button.make ~data:article ()
                          else Article_follow_button.make ~data:follow ~onClick:onFollowClick ()
                          );
                          ( if isAuthor then Article_delete_button.make ~isBusy:isDeleteBusy ~onClick:onDeleteClick ()
                          else Article_favorite_button.make ~data:favorite ~onClick:onFavoriteClick ()
                          );
                        ]
                      ();
                  ]
                ();
            ]
          ();
        div ~className:"container page"
          ~children:
            [
              div ~className:"row article-content"
                ~children:
                  [
                    div ~className:"col-md-12"
                      ~children:
                        [
                          div
                            ~style:React.Dom.Style.(make [| marginBottom "2rem" |])
                            ~children:
                              [
                                ( match article with
                                | Init | Loading -> Spinner.make ()
                                | Reloading (Ok { body; _ }) | Complete (Ok { body; _ }) ->
                                  div ~dangerouslySetInnerHTML:(React.Dom.makeInnerHtml ~__html:body) ()
                                | Reloading (Error _error) | Complete (Error _error) -> "ERROR" |> React.string
                                );
                              ]
                            ();
                          ( match article with
                          | Init | Loading | Reloading (Error _) | Complete (Error _) -> React.null
                          | Reloading (Ok { tagList; _ }) | Complete (Ok { tagList; _ }) ->
                            Article_tag_list.make ~data:tagList ()
                          );
                        ]
                      ();
                  ]
                ();
              hr ();
              div ~className:"article-actions"
                ~children:
                  [
                    div ~className:"article-meta"
                      ~children:
                        [
                          Article_author_avatar.make ~article ();
                          div ~className:"info"
                            ~children:
                              [
                                Article_author_name.make ~article ();
                                span ~className:"date" ~children:[ Article_date.make ~article () ] ();
                              ]
                            ();
                          ( if isAuthor then Article_edit_button.make ~data:article ()
                          else Article_follow_button.make ~data:follow ~onClick:onFollowClick ()
                          );
                          ( if isAuthor then Article_delete_button.make ~isBusy:isDeleteBusy ~onClick:onDeleteClick ()
                          else Article_favorite_button.make ~data:favorite ~onClick:onFavoriteClick ()
                          );
                        ]
                      ();
                  ]
                ();
              div ~className:"row"
                ~children:
                  [
                    div ~className:"col-xs-12 col-md-8 offset-md-2"
                      ~children:
                        [
                          ( match user with
                          | Some { image; _ } ->
                            Article_post_comment.make ~image:(image |> Option.value ~default:"") ~slug ~setComments ()
                          | None ->
                            p
                              ~children:
                                [
                                  Link.make ~className:"nav-link" ~onClick:(Link.login |> Link.location)
                                    ~children:[ "Sign in" |> React.string ]
                                    ();
                                  " or " |> React.string;
                                  Link.make ~className:"nav-link" ~onClick:(Link.register |> Link.location)
                                    ~children:[ "sign up" |> React.string ]
                                    ();
                                  " to add comments on this article." |> React.string;
                                ]
                              ()
                          );
                          Article_comments.make ~slug ~data:comments ~busy:busyComments ~user
                            ~onDeleteClick:deleteComment ();
                        ]
                      ();
                  ]
                ();
            ]
          ();
      ]
    ()
