open React.Dom.Dsl
open Html

external to_input_element : Ojs.t -> Js_of_ocaml.Dom_html.inputElement Js_of_ocaml.Js.t = "%identity"

let value_of_event event = (React.Event.Form.target event |> to_input_element)##.value |> Js_of_ocaml.Js.to_string

let%component make ~(slug : string) ~(image : string)
  ~(setComments :
     ((Shape.comment array, 'a App_error.t) Async_result.t -> (Shape.comment array, 'a App_error.t) Async_result.t) ->
     unit
     )
  =
  let comment, setComment = React.useState (fun () -> Async_data.complete "") in
  let isCommentValid =
    comment |> Async_data.getValue |> Option.map (fun v -> String.trim v <> "") |> Option.value ~default:false
  in
  let body = comment |> Async_data.getValue |> Option.value ~default:"" in
  form ~className:"card comment-form"
    ~children:
      [
        div ~className:"card-block"
          ~children:
            [
              textarea ~className:"form-control" ~placeholder:"Write a comment..." ~rows:3 ~value:body
                ~onChange:(fun event ->
                  let value = value_of_event event in
                  setComment (fun _prev -> Async_data.complete value)
                )
                ~disabled:(comment |> Async_data.isBusy) ~children:[] ();
            ]
          ();
        div ~className:"card-footer"
          ~children:
            [
              ( match image with
              | "" -> img ~className:"comment-author-img" ~children:[] ()
              | src -> img ~src ~className:"comment-author-img" ~children:[] ()
              );
              button ~className:"btn btn-sm btn-primary" ~disabled:(not isCommentValid)
                ~onClick:(fun event ->
                  if isCommentValid && Async_data.isComplete comment then (
                    setComment Async_data.toBusy;
                    Api.addComment ~slug ~body ()
                    |> Promise.then_ ~fulfilled:(fun x ->
                         ( match x with
                         | Ok comment ->
                           setComments (fun prev ->
                             prev |> Async_result.map (fun comments -> Array.append [| comment |] comments)
                           );
                           setComment (fun _prev -> Async_data.complete "")
                         | Error _error -> setComment Async_data.toIdle
                         );
                         Promise.resolve ()
                       )
                    |> Promise.catch ~rejected:(fun _error ->
                         setComment Async_data.toIdle;
                         Promise.resolve ()
                       )
                    |> ignore
                  );
                  event |> React.Event.Mouse.preventDefault;
                  event |> React.Event.Mouse.stopPropagation
                )
                ~children:[ "Post Comment" |> React.string ]
                ();
            ]
          ();
      ]
    ()
