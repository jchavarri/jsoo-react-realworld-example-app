open React.Dom.Dsl
open Html

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
  div
    [| className "article-page" |]
    [
      div
        [| className "banner" |]
        [
          div
            [| className "container" |]
            [
              h1 [||]
                [
                  article
                  |> Async_result.getOk
                  |> Option.map (fun (ok : Shape.article_response) -> ok.title)
                  |> Option.map (fun title -> title |> React.string)
                  |> Option.value ~default:React.null;
                ];
              div
                [| className "article-meta" |]
                [
                  Article_author_avatar.make ~article ();
                  div
                    [| className "info" |]
                    [
                      Article_author_name.make ~article ();
                      span [| className "date" |] [ Article_date.make ~article () ];
                    ];
                  ( if isAuthor then Article_edit_button.make ~data:article ()
                  else Article_follow_button.make ~data:follow ~onClick:onFollowClick ()
                  );
                  ( if isAuthor then Article_delete_button.make ~isBusy:isDeleteBusy ~onClick:onDeleteClick ()
                  else Article_favorite_button.make ~data:favorite ~onClick:onFavoriteClick ()
                  );
                ];
            ];
        ];
      div
        [| className "container page" |]
        [
          div
            [| className "row article-content" |]
            [
              div
                [| className "col-md-12" |]
                [
                  div
                    [| Prop.style React.Dom.Style.(make [| marginBottom "2rem" |]) |]
                    [
                      ( match article with
                      | Init | Loading -> Spinner.make ()
                      | Reloading (Ok { body; _ }) | Complete (Ok { body; _ }) ->
                        div
                          [| dangerouslySetInnerHTML (React.Dom.SafeString.make_unchecked (Markdown.to_html body)) |]
                          []
                      | Reloading (Error _error) | Complete (Error _error) -> "ERROR" |> React.string
                      );
                    ];
                  ( match article with
                  | Init | Loading | Reloading (Error _) | Complete (Error _) -> React.null
                  | Reloading (Ok { tagList; _ }) | Complete (Ok { tagList; _ }) ->
                    Article_tag_list.make ~data:tagList ()
                  );
                ];
            ];
          hr [||] [];
          div
            [| className "article-actions" |]
            [
              div
                [| className "article-meta" |]
                [
                  Article_author_avatar.make ~article ();
                  div
                    [| className "info" |]
                    [
                      Article_author_name.make ~article ();
                      span [| className "date" |] [ Article_date.make ~article () ];
                    ];
                  ( if isAuthor then Article_edit_button.make ~data:article ()
                  else Article_follow_button.make ~data:follow ~onClick:onFollowClick ()
                  );
                  ( if isAuthor then Article_delete_button.make ~isBusy:isDeleteBusy ~onClick:onDeleteClick ()
                  else Article_favorite_button.make ~data:favorite ~onClick:onFavoriteClick ()
                  );
                ];
            ];
          div
            [| className "row" |]
            [
              div
                [| className "col-xs-12 col-md-8 offset-md-2" |]
                [
                  ( match user with
                  | Some { image; _ } ->
                    Article_post_comment.make ~image:(image |> Option.value ~default:"") ~slug ~setComments ()
                  | None ->
                    p [||]
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
                  );
                  Article_comments.make ~slug ~data:comments ~busy:busyComments ~user ~onDeleteClick:deleteComment ();
                ];
            ];
        ];
    ]
