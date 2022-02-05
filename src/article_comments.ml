open React.Dom.Dsl
open Html

let%component make ~(slug : string) ~(data : (Shape.comment array, 'a App_error.t) Async_result.t) ~user
  ~(onDeleteClick : slug:string -> id:int -> unit) ~(busy : Hook.SI.t)
  =
  match data with
  | Init | Loading | Reloading (Error _) -> Spinner.make ()
  | Complete (Error _) -> "ERROR" |> React.string
  | Reloading (Ok comments) | Complete (Ok comments) ->
    fragment
      (comments
      |> Array.map (fun (comment : Shape.comment) ->
           let isAPIBusy = Hook.SI.mem comment.id busy in
           div
             [| className "card"; key (comment.id |> string_of_int) |]
             [
               div [| className "card-block" |] [ p [| className "card-text" |] [ comment.body |> React.string ] ];
               div
                 [| className "card-footer" |]
                 [
                   Link.make
                     ~onClick:(Link.profile ~username:comment.author.username |> Link.location)
                     ~className:"comment-author"
                     ~style:React.Dom.Style.(make [| marginRight "15px" |])
                     ~children:
                       [
                         ( match comment.author.image with
                         | "" -> img [| className "comment-author-img" |] []
                         | src -> img [| Prop.src src; className "comment-author-img" |] []
                         );
                       ]
                     ();
                   Link.make
                     ~onClick:(Link.profile ~username:comment.author.username |> Link.location)
                     ~className:"comment-author"
                     ~children:[ comment.author.username |> React.string ]
                     ();
                   span [| className "date-posted" |] [ comment.createdAt |> Utils.formatDate |> React.string ];
                   span
                     [| className "mod-options" |]
                     [
                       (if false then i [| className "ion-edit" |] [] else React.null);
                       ( match user with
                       | Some ({ username; _ } : Shape.user) when username = comment.author.username ->
                         i
                           [|
                             className (if isAPIBusy then "ion-load-a" else "ion-trash-a");
                             onClick (fun event ->
                               if (not isAPIBusy) && Utils.isMouseRightClick event then
                                 onDeleteClick ~slug ~id:comment.id
                             );
                           |]
                           []
                       | Some _ | None -> React.null
                       );
                     ];
                 ];
             ]
         )
      |> Array.to_list
      )
