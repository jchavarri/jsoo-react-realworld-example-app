[@@@react.dom]

type location'

type onClickAction = Location of location' | CustomFn of (unit -> unit)

let customFn fn = CustomFn fn

let location location = Location location

external make : string -> location' = "%identity"

external toString : location' -> string = "%identity"

let home = make "/"

let settings = make "/#/settings"

let register = make "/#/register"

let login = make "/#/login"

let createArticle = make "/#/editor"

let editArticle ~slug = make ({js|/#/editor/|js} ^ slug)

let article ~slug = make ({js|/#/article/|js} ^ slug)

let profile ~username = make ({js|/#/profile/|js} ^ username)

let favorited ~username =
  make (({js|/#/profile/|js} ^ username) ^ {js|/favorites|js})

let push : location' -> unit =
 fun location -> location |> toString |> React.Router.push

let availableIf : bool -> onClickAction -> onClickAction =
 fun available target -> if available then target else CustomFn ignore

let handleClick onClick event =
  ( match onClick with
  | Location location ->
      if Utils.isMouseRightClick event then (
        event |> React.Event.Mouse.preventDefault ;
        location |> toString |> React.Router.push )
  | CustomFn fn ->
      fn () ) ;
  ignore ()

let%component make ?(className = "") ?(style = React.Dom.Style.make ()) ~onClick
    ~children =
  match onClick with
  | Location location ->
      a ~className ~href:(location |> toString) ~style
        ~onClick:(handleClick onClick) ~children ()
  | CustomFn _fn ->
      a ~className ~style ~onClick:(handleClick onClick) ~children ()

module Button = struct
  let%component make ?(className = "") ?(style = React.Dom.Style.make ())
      ~onClick ?(disabled = false) ~children =
    button ~className ~style ~onClick:(handleClick onClick) ~disabled ~children
      ()
end
