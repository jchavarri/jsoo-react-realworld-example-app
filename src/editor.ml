open React.Dom.Dsl
open Html

external to_input_element : Ojs.t -> Js_of_ocaml.Dom_html.inputElement Js_of_ocaml.Js.t = "%identity"

let value_of_event event = (React.Event.Form.target event |> to_input_element)##.value |> Js_of_ocaml.Js.to_string

let parse_tag_list (str : string) : string list =
  str |> String.split_on_char ',' |> List.map String.trim |> List.filter (fun v -> String.length v > 0)

module Form = struct
  let%component make ~data ~setData ~(onSubmit : Shape.article_response * string -> unit) =
    let isBusy = data |> Async_result.isBusy in
    let error =
      let data = data |> Async_result.getOk in
      Option.bind data (fun (_article, _tagList, error) -> error)
    in
    fragment
      [
        ( match error with
        | None -> React.null
        | Some (error : Shape.editor_error) ->
          ul
            [| className "error-messages" |]
            [
              Error_details.make ~label:"title" ~error:error.title ();
              Error_details.make ~label:"body" ~error:error.body ();
              Error_details.make ~label:"description" ~error:error.description ();
            ]
        );
        form [||]
          [
            fieldset [||]
              [
                fieldset
                  [| className "form-group" |]
                  [
                    input
                      [|
                        type_ "text";
                        className "form-control form-control-lg";
                        placeholder "Article Title";
                        disabled isBusy;
                        value
                          (data
                          |> Async_result.getOk
                          |> Option.map
                               (fun
                                 ( (article : Shape.article_response),
                                   (_tagList : string),
                                   (_error : Shape.editor_error option)
                                 )
                               -> article.title
                             )
                          |> Option.value ~default:""
                          );
                        onChange (fun event ->
                          let title = value_of_event event in
                          setData (fun prev ->
                            prev
                            |> Async_result.map
                                 (fun
                                   ( (article : Shape.article_response),
                                     (tagList : string),
                                     (error : Shape.editor_error option)
                                   )
                                 -> { article with title }, tagList, error
                               )
                          )
                        );
                      |]
                      [];
                  ];
                fieldset
                  [| className "form-group" |]
                  [
                    input
                      [|
                        type_ "text";
                        className "form-control";
                        placeholder "What's this article about?";
                        disabled isBusy;
                        value
                          (data
                          |> Async_result.getOk
                          |> Option.map
                               (fun
                                 ( (article : Shape.article_response),
                                   (_tagList : string),
                                   (_error : Shape.editor_error option)
                                 )
                               -> article.description
                             )
                          |> Option.value ~default:""
                          );
                        onChange (fun event ->
                          let description = value_of_event event in
                          setData (fun prev ->
                            prev
                            |> Async_result.map
                                 (fun
                                   ( (article : Shape.article_response),
                                     (tagList : string),
                                     (error : Shape.editor_error option)
                                   )
                                 -> { article with description }, tagList, error
                               )
                          )
                        );
                      |]
                      [];
                  ];
                fieldset
                  [| className "form-group" |]
                  [
                    textarea
                      [|
                        className "form-control";
                        rows 8;
                        placeholder "Write your article (in markdown)";
                        disabled isBusy;
                        value
                          (data
                          |> Async_result.getOk
                          |> Option.map
                               (fun
                                 ( (article : Shape.article_response),
                                   (_tagList : string),
                                   (_error : Shape.editor_error option)
                                 )
                               -> article.body
                             )
                          |> Option.value ~default:""
                          );
                        onChange (fun event ->
                          let body = value_of_event event in
                          setData (fun prev ->
                            prev
                            |> Async_result.map
                                 (fun
                                   ( (article : Shape.article_response),
                                     (tagList : string),
                                     (error : Shape.editor_error option)
                                   )
                                 -> { article with body }, tagList, error
                               )
                          )
                        );
                      |]
                      [];
                  ];
                fieldset
                  [| className "form-group" |]
                  [
                    input
                      [|
                        type_ "text";
                        className "form-control";
                        placeholder "Enter tags";
                        disabled isBusy;
                        value
                          (data
                          |> Async_result.getOk
                          |> Option.map
                               (fun
                                 ( (_article : Shape.article_response),
                                   (tagList : string),
                                   (_error : Shape.editor_error option)
                                 )
                               -> tagList
                             )
                          |> Option.value ~default:""
                          );
                        onChange (fun event ->
                          let tagList = value_of_event event in
                          setData (fun prev ->
                            prev
                            |> Async_result.map
                                 (fun
                                   ( (article : Shape.article_response),
                                     (_tagList : string),
                                     (error : Shape.editor_error option)
                                   )
                                 -> article, tagList, error
                               )
                          )
                        );
                      |]
                      [];
                    div [| className "tag-list" |] [];
                  ];
                button
                  [|
                    className "btn btn-lg pull-xs-right btn-primary";
                    type_ "button";
                    disabled isBusy;
                    onClick (fun event ->
                      event |> React.Event.Mouse.preventDefault;
                      event |> React.Event.Mouse.stopPropagation;
                      if isBusy then ignore ()
                      else (
                        match data |> Async_result.getOk with
                        | Some
                            ( (article : Shape.article_response),
                              (tagList : string),
                              (_error : Shape.editor_error option)
                            ) ->
                          onSubmit (article, tagList);
                          ignore ()
                        | None -> ignore ()
                      )
                    );
                  |]
                  [ "Publish Article" |> React.string ];
              ];
          ];
      ]
end

module Create = struct
  let empty =
    ( {
        Shape.slug = "";
        title = "";
        description = "";
        body = "";
        tagList = [];
        createdAt = new%js Js_of_ocaml.Js.date_now;
        updatedAt = new%js Js_of_ocaml.Js.date_now;
        favorited = false;
        favoritesCount = 0;
        author = { Shape.username = ""; bio = None; image = ""; following = false };
      },
      "",
      None )

  let%component make () =
    let article, setArticle = React.useState (fun () -> Async_result.completeOk empty) in
    Form.make ~data:article ~setData:setArticle
      ~onSubmit:(fun (article, tagList) ->
        setArticle Async_result.toBusy;
        Api.article ~action:(Create { article with tagList = parse_tag_list tagList }) ()
        |> Promise.then_ ~fulfilled:(fun x ->
             ( match x with
             | Ok (ok : Shape.article_response) ->
               Link.article ~slug:ok.slug |> Link.push;
               setArticle Async_result.toIdle
             | Error (App_error.Fetch (_code, _message, `json json)) ->
               let result = json |> Shape.errors_of_jsobject Shape.editor_error_of_jsobject in
               ( match result with
               | Ok { errors } ->
                 setArticle (fun prev ->
                   prev
                   |> Async_data.toIdle
                   |> Async_result.map (fun (article, tagList, _error) -> article, tagList, Some errors)
                 )
               | Error _e ->
                 Js_of_ocaml.Firebug.console##log "Button.UpdateSettings: failed to decode json";
                 ignore ()
               )
             | Error (Fetch (_, _, `text _)) | Error (Decode _) -> setArticle Async_result.toIdle
             );
             Promise.resolve ()
           )
        |> ignore
      )
      ()
end

module Edit = struct
  let%component make ~(slug : string) =
    let article, setArticle = Hook.useArticle ~slug in
    Form.make ~data:article ~setData:setArticle
      ~onSubmit:(fun (article, tagList) ->
        setArticle Async_result.toBusy;
        Api.article ~action:(Update (slug, { article with tagList = parse_tag_list tagList })) ()
        |> Promise.then_ ~fulfilled:(fun _x ->
             setArticle Async_result.toIdle;
             Promise.resolve ()
           )
        |> ignore
      )
      ()
end

let%component make ?(slug : string option) =
  div
    [| className "editor-page" |]
    [
      div
        [| className "container page" |]
        [
          div
            [| className "row" |]
            [
              div
                [| className "col-md-10 offset-md-1 col-xs-12" |]
                [
                  ( match slug with
                  | Some slug -> Edit.make ~slug ()
                  | None -> Create.make ()
                  );
                ];
            ];
        ];
    ]
