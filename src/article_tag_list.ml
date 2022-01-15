[@@@react.dom]

let%component make ~(data : string list) =
  ul ~className:"tag-list"
    ~children:
      [
        data
        |> List.map (fun tag ->
             li ~key:tag ~className:"tag-default tag-pill tag-outline" ~children:[ tag |> React.string ] ()
           )
        |> React.list;
      ]
    ()
