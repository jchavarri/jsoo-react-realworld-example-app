let%component make ~article =
  article
  |> Async_result.getOk
  |> Option.map (fun (ok : Shape.article_response) -> ok.author)
  |> Option.map (fun (author : Shape.author) ->
       Link.make
         ~onClick:(Link.profile ~username:author.username |> Link.location)
         ~className:"author"
         ~children:[ author.username |> React.string ]
         ()
     )
  |> Option.value ~default:React.null
