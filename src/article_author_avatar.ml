open React.Dom.Dsl
open Html

let%component make ~article =
  article
  |> Async_result.getOk
  |> Option.map (fun (ok : Shape.article_response) -> ok.author)
  |> Option.map (fun (author : Shape.author) ->
       Link.make
         ~onClick:(Link.profile ~username:author.username |> Link.location)
         ~children:
           [
             ( match author.image with
             | "" -> img [||] []
             | src_ -> img [| src src_ |] []
             );
           ]
         ()
     )
  |> Option.value ~default:React.null
