let%component make ~article =
  article
  |> Async_result.getOk
  |> Option.map (fun (ok : Shape.article_response) -> ok.createdAt)
  |> Option.map (fun createdAt -> createdAt |> Utils.formatDate |> React.string)
  |> Option.value ~default:React.null
