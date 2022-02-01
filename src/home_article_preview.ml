open React.Dom.Dsl
open Html

let%component make ~(data : Shape.article_response) ~onToggleFavorite ~isFavoriteBusy =
  div
    [| className "article-preview" |]
    [
      div
        [| className "article-meta" |]
        [
          Link.make
            ~onClick:(Link.profile ~username:data.author.username |> Link.location)
            ~children:[ img [| src data.author.image |] [] ]
            ();
          div
            [| className "info" |]
            [
              Link.make ~className:"author"
                ~onClick:(Link.profile ~username:data.author.username |> Link.location)
                ~children:[ data.author.username |> React.string ]
                ();
              span [| className "date" |] [ data.createdAt##toLocaleString |> Js_of_ocaml.Js.to_string |> React.string ];
            ];
          button
            [|
              className
                ( if data.favorited then "btn btn-primary btn-sm pull-xs-right"
                else "btn btn-outline-primary btn-sm pull-xs-right"
                );
              disabled isFavoriteBusy;
              onClick (fun _event ->
                onToggleFavorite
                  ~action:(if data.favorited then Api.Action.Unfavorite data.slug else Api.Action.Favorite data.slug)
              );
            |]
            [
              i
                [|
                  className (if isFavoriteBusy then "ion-load-a" else "ion-heart");
                  Prop.style React.Dom.Style.(make [| marginRight "3px" |]);
                |]
                [];
              data.favoritesCount |> string_of_int |> React.string;
            ];
        ];
      Link.make
        ~onClick:(Link.article ~slug:data.slug |> Link.location)
        ~className:"preview-link"
        ~children:
          [
            h1 [||] [ data.title |> React.string ];
            p [||] [ data.description |> React.string ];
            span [||] [ "Read more..." |> React.string ];
          ]
        ();
    ]
