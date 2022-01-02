[@@@react.dom]

let%component make () =
  div ~className:"lds-ring"
    ~children:[ div ~children:[] (); div ~children:[] (); div ~children:[] (); div ~children:[] () ]
    ()
