open React.Dom.Dsl
open Html

let%component make ~data =
  data
  |> Async_result.getOk
  |> Option.map (fun (ok : Shape.article_response) ->
       Link.make ~className:"btn btn-outline-secondary btn-sm"
         ~onClick:(Link.editArticle ~slug:ok.slug |> Link.location)
         ~children:
           [
             i [| className "ion-edit"; Prop.style React.Dom.Style.(make [| marginRight "5px" |]) |] [];
             "Edit Article" |> React.string;
           ]
         ()
     )
  |> Option.value ~default:React.null
