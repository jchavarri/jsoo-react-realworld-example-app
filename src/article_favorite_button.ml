open React.Dom.Dsl
open Html

let%component make ~(data : (bool * int * string) Async_data.t) ~(onClick : Link.onClickAction) =
  Link.Button.make
    ~className:
      ( match data with
      | Init | Loading | Reloading (false, _, _) | Complete (false, _, _) -> "btn btn-sm btn-outline-primary"
      | Reloading (true, _, _) | Complete (true, _, _) -> "btn btn-sm btn-primary"
      )
    ~style:React.Dom.Style.(make [| marginLeft "5px" |])
    ~onClick:
      ( match data with
      | Init | Loading | Reloading (_, _, _) -> Link.customFn ignore
      | Complete (_, _, _) -> onClick
      )
    ~children:
      [
        i
          [|
            className (if Async_data.isBusy data then "ion-load-a" else "ion-heart");
            Prop.style React.Dom.Style.(make [| marginRight "5px" |]);
          |]
          [];
        ( match data with
        | Init | Loading -> React.null
        | Reloading (favorited, favoritesCount, _slug) | Complete (favorited, favoritesCount, _slug) ->
          React.Fragment.make
            ~children:
              [
                (if favorited then "Unfavorite Article " else "Favorite Article ") |> React.string;
                span [| className "counter" |] [ "(" ^ (favoritesCount |> string_of_int) ^ ")" |> React.string ];
              ]
            ()
        );
      ]
    ()
