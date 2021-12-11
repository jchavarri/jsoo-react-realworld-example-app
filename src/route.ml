type t =
  | Home
  | Login
  | Register
  | CreateArticle
  | EditArticle of string
  | Article of string
  | Profile of string
  | Favorited of string
  | Settings

let useRoute : unit -> t =
 fun () ->
  let url = React.Router.useUrl () in
  let hash = url.hash |> String.split_on_char '/' in
  match hash with
  | [""; "settings"] ->
      Settings
  | [""; "login"] ->
      Login
  | [""; "register"] ->
      Register
  | [""; "editor"] ->
      CreateArticle
  | [""; "editor"; slug] ->
      EditArticle slug
  | [""; "article"; slug] ->
      Article slug
  | [""; "profile"; username] ->
      Profile username
  | [""; "profile"; username; "favorites"] ->
      Favorited username
  | _ ->
      Home
