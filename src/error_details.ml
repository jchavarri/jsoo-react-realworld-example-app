let%component make ~label ~error =
  error
  |> Option.map (fun message ->
       let children =
         message
         |> Array.map (fun message ->
              (li ~key:message ~children:[ ((label ^ {js| |js}) ^ message |> React.string) [@ns.braces] ] () [@JSX])
            )
         |> Array.to_list
       in
       React.Fragment.make ~children ()
     )
  |> Option.value ~default:React.null
