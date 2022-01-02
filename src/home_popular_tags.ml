[@@@react.dom]

let%component make ~(data : (string array, 'a App_error.t) Async_result.t) ~onClick =
  React.Fragment.make
    ~children:
      [
        p ~children:[ "Popular Tags" |> React.string ] ();
        With_test_id.make ~id:"tag-list"
          ~children:
            (div ~className:"tag-list"
               ~children:
                 ( match data with
                 | Init -> [ React.string "Initilizing..." ]
                 | Loading -> [ React.string "Loading..." ]
                 | Reloading (Ok tags) | Complete (Ok tags) ->
                   tags
                   |> Array.map (fun tag ->
                        a ~key:tag ~href:"#" ~className:"tag-pill tag-default"
                          ~onClick:(fun event ->
                            if Utils.isMouseRightClick event then (
                              event |> React.Event.Mouse.preventDefault;
                              tag |> onClick |> ignore
                            )
                          )
                          ~children:[ tag |> React.string ]
                          ()
                      )
                   |> Array.to_list
                 | Reloading (Error _error) | Complete (Error _error) -> [ React.string "ERROR" ]
                 )
               ()
            )
          ();
      ]
    ()
