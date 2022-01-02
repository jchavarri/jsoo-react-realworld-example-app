[@@@react.dom]

let%component make ~(data : Shape.article_response) ~onToggleFavorite ~isFavoriteBusy =
  div ~className:"article-preview"
    ~children:
      [
        div ~className:"article-meta"
          ~children:
            [
              Link.make
                ~onClick:(Link.profile ~username:data.author.username |> Link.location)
                ~children:[ img ~src:data.author.image ~children:[] () ]
                ();
              div ~className:"info"
                ~children:
                  [
                    Link.make ~className:"author"
                      ~onClick:(Link.profile ~username:data.author.username |> Link.location)
                      ~children:[ data.author.username |> React.string ]
                      ();
                    span ~className:"date"
                      ~children:[ data.createdAt##toLocaleString |> Js_of_ocaml.Js.to_string |> React.string ]
                      ();
                  ]
                ();
              button
                ~className:
                  ( if data.favorited then "btn btn-primary btn-sm pull-xs-right"
                  else "btn btn-outline-primary btn-sm pull-xs-right"
                  )
                ~disabled:isFavoriteBusy
                ~onClick:(fun _event ->
                  onToggleFavorite
                    ~action:(if data.favorited then Api.Action.Unfavorite data.slug else Api.Action.Favorite data.slug)
                )
                ~children:
                  [
                    i
                      ~className:(if isFavoriteBusy then "ion-load-a" else "ion-heart")
                      ~style:(React.Dom.Style.make ~marginRight:"3px" ())
                      ~children:[] ();
                    data.favoritesCount |> string_of_int |> React.string;
                  ]
                ();
            ]
          ();
        Link.make
          ~onClick:(Link.article ~slug:data.slug |> Link.location)
          ~className:"preview-link"
          ~children:
            [
              h1 ~children:[ data.title |> React.string ] ();
              p ~children:[ data.description |> React.string ] ();
              span ~children:[ "Read more..." |> React.string ] ();
            ]
          ();
      ]
    ()
