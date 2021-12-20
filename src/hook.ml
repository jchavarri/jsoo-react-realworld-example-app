open Promise

type nonrec asyncData = Shape.user option Async_data.t

let useCurrentUser : unit -> asyncData * ((asyncData -> asyncData) -> unit) =
 fun () ->
  let data, setData = React.useState (fun () -> Async_data.init) in
  React.useEffect0 (fun () ->
    setData (fun prev -> prev |> Async_data.toBusy);
    Api.currentUser ()
    |> then_ ~fulfilled:(fun data ->
         setData (fun _prev ->
           match data with
           | Ok data' -> Some data' |> Async_data.complete
           | Error _error -> None |> Async_data.complete
         )
         |> resolve
       )
    |> catch ~rejected:(fun _error -> setData (fun _prev -> None |> Async_data.complete) |> resolve)
    |> ignore;
    None
  );
  data, setData
