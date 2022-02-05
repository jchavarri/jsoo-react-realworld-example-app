open React.Dom.Dsl
open Html

let%component make ~(data : string list) =
  ul
    [| className "tag-list" |]
    (data
    |> List.map (fun tag -> li [| key tag; className "tag-default tag-pill tag-outline" |] [ tag |> React.string ])
    )
