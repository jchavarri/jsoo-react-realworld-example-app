[@@@react.dom]

external to_input_element : Ojs.t -> Js_of_ocaml.Dom_html.inputElement Js_of_ocaml.Js.t = "%identity"

let value_of_event event = (React.Event.Form.target event |> to_input_element)##.value |> Js_of_ocaml.Js.to_string

let%component make ~setUser =
  let data, setData = React.useState (fun () -> Async_data.complete ("", "", None)) in
  let isBusy = data |> Async_data.isBusy in
  let email, password, error = data |> Async_data.getValue |> Option.value ~default:("", "", None) in
  div ~className:"auth-page"
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
                          h1 ~className:"text-xs-center" ~children:[ "Sign in" |> React.string ] ();
                          p ~className:"text-xs-center"
                            ~children:
                              [
                                Link.make ~onClick:(Link.register |> Link.location)
                                  ~children:[ "Need an account?" |> React.string ] ();
                              ]
                            ();
                          ( match error with
                          | Some messages ->
                            ul ~className:"error-messages"
                              ~children:
                                [
                                  messages
                                  |> List.map (fun message ->
                                       li ~key:message
                                         ~children:[ {js|email or password |js} ^ message |> React.string ]
                                         ()
                                     )
                                  |> React.list;
                                ]
                              ()
                          | None -> React.null
                          );
                          form
                            ~children:
                              [
                                fieldset ~className:"form-group"
                                  ~children:
                                    [
                                      input ~className:"form-control form-control-lg" ~type_:"text" ~placeholder:"Email"
                                        ~value:email ~disabled:isBusy
                                        ~onChange:(fun event ->
                                          let email = value_of_event event in
                                          setData (fun prev ->
                                            prev
                                            |> Async_data.map (fun (_email, password, error) -> email, password, error)
                                          )
                                        )
                                        ~children:[] ();
                                    ]
                                  ();
                                fieldset ~className:"form-group"
                                  ~children:
                                    [
                                      input ~className:"form-control form-control-lg" ~type_:"password"
                                        ~placeholder:"Password" ~value:password ~disabled:isBusy
                                        ~onChange:(fun event ->
                                          let password = value_of_event event in
                                          setData (fun prev ->
                                            prev
                                            |> Async_data.map (fun (email, _password, error) -> email, password, error)
                                          )
                                        )
                                        ~children:[] ();
                                    ]
                                  ();
                                button ~className:"btn btn-lg btn-primary pull-xs-right" ~disabled:isBusy
                                  ~onClick:(fun event ->
                                    event |> React.Event.Mouse.preventDefault;
                                    event |> React.Event.Mouse.stopPropagation;
                                    setData Async_data.toBusy;
                                    Api.login ~email ~password ()
                                    |> Promise.then_ ~fulfilled:(fun x ->
                                         ( match x with
                                         | Ok (user : Shape.user) ->
                                           setUser (fun _prev -> Some user |> Async_data.complete);
                                           setData Async_data.toIdle;
                                           Utils.setCookie "jwtToken" (Some user.token);
                                           Link.home |> Link.push
                                         | Error (App_error.Fetch (_code, _message, `json json)) ->
                                           ( try
                                               let result =
                                                 json |> Shape.errors_of_jsobject Shape.login_error_of_jsobject
                                               in
                                               match result with
                                               | Ok { errors } ->
                                                 setData (fun prev ->
                                                   prev
                                                   |> Async_data.toIdle
                                                   |> Async_data.map (fun (email, password, _error) ->
                                                        email, password, errors
                                                      )
                                                 )
                                               | Error _e -> ignore ()
                                             with _ ->
                                               Js_of_ocaml.Firebug.console##log "Button.SignIn: failed to decode json"
                                           )
                                         | Error (Fetch (_, _, `text _)) | Error (Decode _) -> setData Async_data.toIdle
                                         );
                                         Promise.resolve ()
                                       )
                                    |> ignore
                                  )
                                  ~children:[ "Sign in" |> React.string ] ();
                              ]
                            ();
                        ]
                      ();
                  ]
                ();
            ]
          ();
      ]
    ()
