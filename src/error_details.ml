[@@@react.dom]

let%component make ~label ~error =
  error
  |> Option.map (fun message ->
       let children =
         message
         |> Array.map (fun message -> li ~key:message ~children:[ label ^ " " ^ message |> React.string ] ())
         |> Array.to_list
       in
       React.Fragment.make ~children ()
     )
  |> Option.value ~default:React.null
