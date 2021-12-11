[@@@react.dom]

let%component make ~user =
  let currentUser =
    match user with
    | Some user ->
        user
    | None ->
        {Shape_t.email= ""; username= ""; bio= None; image= None; token= ""}
  in
  nav ~className:"navbar navbar-light"
    ~children:
      [ div ~className:"container"
          ~children:
            [ Link.make ~className:"navbar-brand"
                ~onClick:(Link.location Link.home)
                ~children:[React.string "conduit"] ()
            ; ul ~className:"nav navbar-nav pull-xs-right"
                ~children:
                  [ li ~className:"nav-item"
                      ~children:
                        [ Link.make ~className:"nav-link active"
                            ~onClick:(Link.location Link.home)
                            ~children:[React.string "Home"] () ]
                      ()
                  ; Security.AnonymousOnly.make ~user
                      ~children:
                        [ li ~className:"nav-item"
                            ~children:
                              [ Link.make ~className:"nav-link"
                                  ~onClick:(Link.location Link.login)
                                  ~children:[React.string "Sign in"] () ]
                            ()
                        ; li ~className:"nav-item"
                            ~children:
                              [ Link.make ~className:"nav-link"
                                  ~onClick:(Link.location Link.register)
                                  ~children:[React.string "Sign up"] () ]
                            () ]
                      ()
                  ; Security.AuthenticatedOnly.make ~user
                      ~children:
                        [ li ~className:"nav-item"
                            ~children:
                              [ Link.make ~className:"nav-link"
                                  ~onClick:(Link.location Link.createArticle)
                                  ~children:
                                    [ i ~className:"ion-compose" ~children:[] ()
                                    ; React.string " New Post" ]
                                  () ]
                            ()
                        ; li ~className:"nav-item"
                            ~children:
                              [ Link.make ~className:"nav-link"
                                  ~onClick:(Link.location Link.settings)
                                  ~children:
                                    [ i ~className:"ion-gear-a" ~children:[] ()
                                    ; React.string " Settings" ]
                                  () ]
                            ()
                        ; li ~className:"nav-item"
                            ~children:
                              [ Link.make ~className:"nav-link"
                                  ~onClick:
                                    ( Link.profile ~username:currentUser.username
                                    |> Link.location )
                                  ~children:[React.string currentUser.username]
                                  () ]
                            () ]
                      () ]
                () ]
          () ]
    ()
