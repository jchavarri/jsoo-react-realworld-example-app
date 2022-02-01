open React.Dom.Dsl
open Html

let%component make ~isBusy ~onClick =
  Link.Button.make ~className:"btn btn-outline-danger btn-sm" ~onClick
    ~style:React.Dom.Style.(make [| marginLeft "5px" |])
    ~children:
      [
        i
          ~className:(if isBusy then "ion-load-a" else "ion-trash-a")
          ~style:React.Dom.Style.(make [| marginRight "5px" |])
          ~children:[] ();
        "Delete Article" |> React.string;
      ]
    ()
