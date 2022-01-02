[@@@react.dom]

let%component make ~(limit : int) ~(offset : int) ~(total : int) ~(onClick : int -> unit) =
  if total = 0 then React.null
  else (
    let pages = Js_of_ocaml.Js.math##ceil (float_of_int total /. float_of_int limit) |> int_of_float |> ( - ) 1 in
    With_test_id.make ~id:"page-link"
      ~children:
        (ul ~className:"pagination"
           ~children:
             (List.init pages (fun x -> x + 1)
             |> List.map (fun page ->
                  let className =
                    if (offset = 0 && page = 0) || page = offset / limit then "page-item active" else "page-item"
                  in
                  li ~key:(page |> string_of_int) ~className
                    ~children:
                      [
                        a ~className:"page-link"
                          ~href:({js|#|js} ^ (page |> string_of_int))
                          ~onClick:(fun event ->
                            if Utils.isMouseRightClick event then (
                              event |> React.Event.Mouse.preventDefault;
                              onClick (page * limit)
                            )
                          )
                          ~children:[ string_of_int (page + 1) |> React.string ]
                          ();
                      ]
                    ()
                )
             )
           ()
        )
      ()
  )
