type nonrec asyncData = Shape_t.user option Async_data.t

let useCurrentUser : unit -> asyncData * ((asyncData -> asyncData) -> unit) =
 fun () ->
  let data, setData = React.useState (fun () -> Async_data.init) in
  (* React.useEffect0 (fun () ->
      setData (fun prev -> prev |. AsyncData.toBusy) ;
      API.currentUser ()
      |. then_ (fun data ->
             setData (fun _prev ->
                 match data with
                 | Ok data' ->
                     Some data' |. AsyncData.complete
                 | Error _error ->
                     None |. AsyncData.complete )
             |. resolve )
      |. catch (fun _error ->
             setData (fun _prev -> None |. AsyncData.complete) |. resolve )
      |. ignore ;
      None ) ; *)
  (data, setData)
