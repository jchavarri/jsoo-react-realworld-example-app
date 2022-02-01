open React.Dom.Dsl
open Html

let%component make () =
  fragment
    [
      a
        [|
          href "https://github.com/jihchi/jsoo-react-realworld-example-app";
          target "_blank";
          Prop.style
            React.Dom.Style.(
              make
                [|
                  position "fixed";
                  bottom "0";
                  width "100%";
                  background "linear-gradient(#485563, #29323c)";
                  textAlign "center";
                  padding "15px";
                  boxShadow "0 5px 5px 5px rgba(0,0,0,0.4)";
                  zIndex "999";
                  fontSize "1.5rem";
                  display "block";
                  color "#fff";
                |]
            );
        |]
        [
          i [| className "ion-social-github"; Prop.style React.Dom.Style.(make [| marginRight "8px" |]) |] [];
          React.string "Fork on GitHub";
        ];
      footer [||]
        [
          div
            [| className "container" |]
            [
              Link.make ~onClick:(Link.location Link.home) ~className:"logo-font"
                ~children:[ React.string "conduit" ]
                ();
              span
                [| className "attribution" |]
                [
                  React.string "An interactive learning project from ";
                  a [| href "https://thinkster.io" |] [ React.string "Thinkster" ];
                  React.string ". Code &amp; design licensed under MIT.";
                ];
            ];
        ];
    ]
