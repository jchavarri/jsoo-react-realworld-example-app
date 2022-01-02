[@@@react.dom]

external to_input_element : Ojs.t -> Js_of_ocaml.Dom_html.inputElement Js_of_ocaml.Js.t = "%identity"

let value_of_event event = (React.Event.Form.target event |> to_input_element)##.value |> Js_of_ocaml.Js.to_string

let%component make ~(user : Shape.user)
  ~(setUser : (Shape.user option Async_data.t -> Shape.user option Async_data.t) -> unit)
  =
  let result, setResult = React.useState (fun () -> Async_data.complete (user, "", None)) in
  let isBusy = result |> Async_data.isBusy in
  let form, password, error = result |> Async_data.getValue |> Option.value ~default:(user, "", None) in
  div ~className:"settings-page"
    ~children:
      [
        div ~className:"container page"
          ~children:
            [
              div ~className:"row"
                ~children:
                  [
                    div ~className:"col-md-6 offset-md-3 col-xs-12"
                      ~children:
                        [
                          h1 ~className:"text-xs-center" ~children:[ "Your Settings" |> React.string ] ();
                          ( match error with
                          | None -> React.null
                          | Some (error : Shape.settings) ->
                            ul ~className:"error-messages"
                              ~children:
                                [
                                  Error_details.make ~label:"email" ~error:error.email ();
                                  Error_details.make ~label:"bio" ~error:error.bio ();
                                  Error_details.make ~label:"image" ~error:error.image ();
                                  Error_details.make ~label:"username" ~error:error.username ();
                                  Error_details.make ~label:"password" ~error:error.password ();
                                ]
                              ()
                          );
                          form
                            ~children:
                              [
                                fieldset
                                  ~children:
                                    [
                                      fieldset ~className:"form-group"
                                        ~children:
                                          [
                                            input ~className:"form-control" ~type_:"text"
                                              ~placeholder:"URL of profile picture" ~disabled:isBusy
                                              ~value:(form.image |> Option.value ~default:"")
                                              ~onChange:(fun event ->
                                                let image = value_of_event event in
                                                setResult (fun prev ->
                                                  prev
                                                  |> Async_data.map (fun ((user : Shape.user), password, error) ->
                                                       { user with image = Some image }, password, error
                                                     )
                                                )
                                              )
                                              ~children:[] ();
                                          ]
                                        ();
                                      fieldset ~className:"form-group"
                                        ~children:
                                          [
                                            input ~className:"form-control form-control-lg" ~type_:"text"
                                              ~placeholder:"Your Name" ~disabled:isBusy ~value:form.username
                                              ~onChange:(fun event ->
                                                let username = value_of_event event in
                                                setResult (fun prev ->
                                                  prev
                                                  |> Async_data.map (fun ((user : Shape.user), password, error) ->
                                                       { user with username }, password, error
                                                     )
                                                )
                                              )
                                              ~children:[] ();
                                          ]
                                        ();
                                      fieldset ~className:"form-group"
                                        ~children:
                                          [
                                            textarea ~className:"form-control form-control-lg" ~rows:8
                                              ~placeholder:"Short bio about you" ~disabled:isBusy
                                              ~value:(form.bio |> Option.value ~default:"")
                                              ~onChange:(fun event ->
                                                let bio = value_of_event event in
                                                setResult (fun prev ->
                                                  prev
                                                  |> Async_data.map (fun ((user : Shape.user), password, error) ->
                                                       { user with bio = Some bio }, password, error
                                                     )
                                                )
                                              )
                                              ~children:[] ();
                                          ]
                                        ();
                                      fieldset ~className:"form-group"
                                        ~children:
                                          [
                                            input ~className:"form-control form-control-lg" ~type_:"text"
                                              ~placeholder:"Email" ~disabled:isBusy ~value:form.email
                                              ~onChange:(fun event ->
                                                let email = value_of_event event in
                                                setResult (fun prev ->
                                                  prev
                                                  |> Async_data.map (fun ((user : Shape.user), password, error) ->
                                                       { user with email }, password, error
                                                     )
                                                )
                                              )
                                              ~children:[] ();
                                          ]
                                        ();
                                      fieldset ~className:"form-group"
                                        ~children:
                                          [
                                            input ~className:"form-control form-control-lg" ~type_:"password"
                                              ~placeholder:"Password" ~disabled:isBusy ~value:password
                                              ~onChange:(fun event ->
                                                let password = value_of_event event in
                                                setResult (fun prev ->
                                                  prev
                                                  |> Async_data.map (fun (user, _password, error) ->
                                                       user, password, error
                                                     )
                                                )
                                              )
                                              ~children:[] ();
                                          ]
                                        ();
                                      button ~className:"btn btn-lg btn-primary pull-xs-right" ~disabled:isBusy
                                        ~onClick:(fun event ->
                                          event |> React.Event.Mouse.preventDefault;
                                          event |> React.Event.Mouse.stopPropagation;
                                          result
                                          |> Async_data.tapComplete (fun (user, password, _error) ->
                                               setResult Async_data.toBusy;
                                               Api.updateUser ~user ~password ()
                                               |> Promise.then_ ~fulfilled:(fun res ->
                                                    ( match res with
                                                    | Ok user ->
                                                      setResult (fun prev ->
                                                        prev
                                                        |> Async_data.toIdle
                                                        |> Async_data.map (fun (_user, _password, _error) ->
                                                             user, "", None
                                                           )
                                                      );
                                                      setUser (fun prev ->
                                                        prev |> Async_data.map (fun _prev -> Some user)
                                                      )
                                                    | Error (App_error.Fetch (_code, _message, `json json)) ->
                                                      ( try
                                                          let result =
                                                            json |> Shape.errors_of_jsobject Shape.settings_of_jsobject
                                                          in
                                                          match result with
                                                          | Ok { errors } ->
                                                            setResult (fun prev ->
                                                              prev
                                                              |> Async_data.toIdle
                                                              |> Async_data.map (fun (user, _password, _error) ->
                                                                   user, "", Some errors
                                                                 )
                                                            )
                                                          | Error _e -> ignore ()
                                                        with _ ->
                                                          Js_of_ocaml.Firebug.console##log
                                                            "Button.UpdateSettings: failed to decode json";
                                                          ignore ()
                                                      )
                                                    | Error (Fetch (_, _, `text _)) | Error (Decode _) -> ignore ()
                                                    );
                                                    Promise.resolve ()
                                                  )
                                               |> ignore
                                             )
                                          |> ignore
                                        )
                                        ~children:[ "Update Settings" |> React.string ]
                                        ();
                                    ]
                                  ();
                              ]
                            ();
                          hr ~children:[] ();
                          button ~className:"btn btn-outline-danger" ~disabled:isBusy
                            ~onClick:(fun event ->
                              event |> React.Event.Mouse.preventDefault;
                              event |> React.Event.Mouse.stopPropagation;
                              if isBusy then ignore ()
                              else (
                                setUser (fun _prev -> Async_data.complete None);
                                Utils.deleteCookie "jwtToken";
                                Link.home |> Link.push
                              )
                            )
                            ~children:[ "Or click here to logout." |> React.string ]
                            ();
                        ]
                      ();
                  ]
                ();
            ]
          ();
      ]
    ()
