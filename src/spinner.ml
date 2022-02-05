open React.Dom.Dsl
open Html

let%component make () = div [| className "lds-ring" |] [ div [||] []; div [||] []; div [||] []; div [||] [] ]
