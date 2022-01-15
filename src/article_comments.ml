[@@@react.dom]

let%component make ~(slug : string) ~(data : (Shape.comment array, 'a App_error.t) Async_result.t) ~user
  ~(onDeleteClick : slug:string -> id:int -> unit) ~(busy : Hook.SI.t)
  =
  match data with
  | Init | Loading | Reloading (Error _) -> Spinner.make ()
  | Complete (Error _) -> "ERROR" |> React.string
  | Reloading (Ok comments) | Complete (Ok comments) ->
    comments
    |> Array.map (fun (comment : Shape.comment) ->
         let isAPIBusy = Hook.SI.mem comment.id busy in
         div ~className:"card" ~key:(comment.id |> string_of_int)
           ~children:
             [
               div ~className:"card-block"
                 ~children:[ p ~className:"card-text" ~children:[ comment.body |> React.string ] () ]
                 ();
               div ~className:"card-footer"
                 ~children:
                   [
                     Link.make
                       ~onClick:(Link.profile ~username:comment.author.username |> Link.location)
                       ~className:"comment-author"
                       ~style:(React.Dom.Style.make ~marginRight:"7px" ())
                       ~children:
                         [
                           ( match comment.author.image with
                           | "" -> img ~className:"comment-author-img" ~children:[] ()
                           | src -> img ~src ~className:"comment-author-img" ~children:[] ()
                           );
                         ]
                       ();
                     Link.make
                       ~onClick:(Link.profile ~username:comment.author.username |> Link.location)
                       ~className:"comment-author"
                       ~children:[ comment.author.username |> React.string ]
                       ();
                     span ~className:"date-posted"
                       ~children:[ comment.createdAt |> Utils.formatDate |> React.string ]
                       ();
                     span ~className:"mod-options"
                       ~children:
                         [
                           (if false then i ~className:"ion-edit" ~children:[] () else React.null);
                           ( match user with
                           | Some ({ username; _ } : Shape.user) when username = comment.author.username ->
                             i
                               ~className:(if isAPIBusy then "ion-load-a" else "ion-trash-a")
                               ~onClick:(fun event ->
                                 if (not isAPIBusy) && Utils.isMouseRightClick event then
                                   onDeleteClick ~slug ~id:comment.id
                               )
                               ~children:[] ()
                           | Some _ | None -> React.null
                           );
                         ]
                       ();
                   ]
                 ();
             ]
           ()
       )
    |> Array.to_list
    |> React.list
