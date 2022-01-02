[@@@react.dom]

type t = {
  username : string;
  email : string;
  password : string;
}

external to_input_element : Ojs.t -> Js_of_ocaml.Dom_html.inputElement Js_of_ocaml.Js.t = "%identity"

let value_of_event event = (React.Event.Form.target event |> to_input_element)##.value |> Js_of_ocaml.Js.to_string

let empty = { username = ""; email = ""; password = "" }, None

let%component make ~setUser =
  let data, setData = React.useState (fun () -> Async_data.complete empty) in
  let isBusy = data |> Async_data.isBusy in
  let form, error = data |> Async_data.getValue |> Option.value ~default:empty in
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
                          h1 ~className:"text-xs-center" ~children:[ "Sign up" |> React.string ] ();
                          p ~className:"text-xs-center"
                            ~children:
                              [
                                Link.make ~onClick:(Link.login |> Link.location)
                                  ~children:[ "Have an account?" |> React.string ]
                                  ();
                              ]
                            ();
                          ( match error with
                          | Some (detail : Shape.register_error) ->
                            ul ~className:"error-messages"
                              ~children:
                                [
                                  Error_details.make ~label:"email" ~error:detail.email ();
                                  Error_details.make ~label:"password" ~error:detail.password ();
                                  Error_details.make ~label:"username" ~error:detail.username ();
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
                                      input ~className:"form-control form-control-lg" ~type_:"text"
                                        ~placeholder:"Your Name" ~disabled:isBusy ~value:form.username
                                        ~onChange:(fun event ->
                                          let username = value_of_event event in
                                          setData (fun prev ->
                                            prev |> Async_data.map (fun (form, error) -> { form with username }, error)
                                          )
                                        )
                                        ~children:[] ();
                                    ]
                                  ();
                                fieldset ~className:"form-group"
                                  ~children:
                                    [
                                      input ~className:"form-control form-control-lg" ~type_:"text" ~placeholder:"Email"
                                        ~disabled:isBusy ~value:form.email
                                        ~onChange:(fun event ->
                                          let email = value_of_event event in
                                          setData (fun prev ->
                                            prev |> Async_data.map (fun (form, error) -> { form with email }, error)
                                          )
                                        )
                                        ~children:[] ();
                                    ]
                                  ();
                                fieldset ~className:"form-group"
                                  ~children:
                                    [
                                      input ~className:"form-control form-control-lg" ~type_:"password"
                                        ~placeholder:"Password" ~disabled:isBusy ~value:form.password
                                        ~onChange:(fun event ->
                                          let password = value_of_event event in
                                          setData (fun prev ->
                                            prev |> Async_data.map (fun (form, error) -> { form with password }, error)
                                          )
                                        )
                                        ~children:[] ();
                                    ]
                                  ();
                                button ~className:"btn btn-lg btn-primary pull-xs-right" ~disabled:isBusy
                                  ~onClick:(fun event ->
                                    event |> React.Event.Mouse.preventDefault;
                                    event |> React.Event.Mouse.stopPropagation;
                                    if isBusy then ignore ()
                                    else (
                                      setData Async_data.toBusy;
                                      Api.register ~username:form.username ~email:form.email ~password:form.password ()
                                      |> Promise.then_ ~fulfilled:(fun x ->
                                           ( match x with
                                           | Ok (user : Shape.user) ->
                                             setUser (fun _prev -> Some user |> Async_data.complete);
                                             setData Async_data.toIdle;
                                             Utils.setCookie "jwtToken" (Some user.token);
                                             Link.home |> Link.push
                                           | Error (App_error.Fetch (_code, _message, `json json)) ->
                                             let result =
                                               json |> Shape.errors_of_jsobject Shape.register_error_of_jsobject
                                             in
                                             ( match result with
                                             | Ok { errors } ->
                                               setData (fun prev ->
                                                 prev
                                                 |> Async_data.toIdle
                                                 |> Async_data.map (fun (form, _error) -> form, Some errors)
                                               )
                                             | Error _e ->
                                               Js_of_ocaml.Firebug.console##log
                                                 "Button.UpdateSettings: failed to decode json";
                                               ignore ()
                                             )
                                           | Error (Fetch (_, _, `text _)) | Error (Decode _) ->
                                             setData Async_data.toIdle
                                           );
                                           Promise.resolve ()
                                         )
                                      |> ignore
                                    )
                                  )
                                  ~children:[ "Sign up" |> React.string ]
                                  ();
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
