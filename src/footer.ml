[@@@react.dom]

let%component make () =
  React.Fragment.make
    ~children:
      [
        a ~href:"https://github.com/jihchi/jsoo-react-realworld-example-app" ~target:"_blank"
          ~style:
            (React.Dom.Style.make ~position:"fixed" ~bottom:"0" ~width:"100%"
               ~background:"linear-gradient(#485563, #29323c)" ~textAlign:"center" ~padding:"15px"
               ~boxShadow:"0 5px 5px 5px rgba(0,0,0,0.4)" ~zIndex:"999" ~fontSize:"1.5rem" ~display:"block"
               ~color:"#fff" ()
            )
          ~children:
            [
              i ~className:"ion-social-github" ~style:(React.Dom.Style.make ~marginRight:"8px" ()) ~children:[] ();
              React.string "Fork on GitHub";
            ]
          ();
        footer
          ~children:
            [
              div ~className:"container"
                ~children:
                  [
                    Link.make ~onClick:(Link.location Link.home) ~className:"logo-font"
                      ~children:[ React.string "conduit" ]
                      ();
                    span ~className:"attribution"
                      ~children:
                        [
                          React.string "An interactive learning project from ";
                          a ~href:"https://thinkster.io" ~children:[ React.string "Thinkster" ] ();
                          React.string ". Code &amp; design licensed under MIT.";
                        ]
                      ();
                  ]
                ();
            ]
          ();
      ]
    ()
