open React.Dom.Dsl
open Html

let%component make ~(data : (string * bool) Async_data.t) ~(onClick : Link.onClickAction) =
  Link.Button.make
    ~className:
      ( match data with
      | Init | Loading | Reloading (_, false) | Complete (_, false) -> "btn btn-sm btn-outline-secondary"
      | Reloading (_, true) | Complete (_, true) -> "btn btn-sm btn-secondary"
      )
    ~onClick:
      ( match data with
      | Init | Loading | Reloading (_, _) -> Link.customFn ignore
      | Complete (_, _) -> onClick
      )
    ~children:
      [
        i
          ~className:(if Async_data.isBusy data then "ion-load-a" else "ion-plus-round")
          ~style:React.Dom.Style.(make [| marginRight "5px" |])
          ~children:[] ();
        ( match data with
        | Init | Loading -> React.null
        | Reloading (username, following) | Complete (username, following) ->
          ((if following then "Unfollow" else "Follow") ^ " ") ^ username |> React.string
        );
      ]
    ()
