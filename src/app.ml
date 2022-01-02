let authenticated : (Shape.user -> React.element) -> Shape.user option -> React.element =
 fun getPage user ->
  match user with
  | Some s -> getPage s
  | None ->
    Link.home |> Link.push;
    React.null

let%component make () =
  let currentUser, setCurrentUser = Hook.useCurrentUser () in
  let route = Route.useRoute () in
  match currentUser with
  | Init | Loading -> React.null
  | Reloading user | Complete user ->
    React.Fragment.make
      ~children:
        [
          Header.make ~user ();
          ( match route with
          | Settings -> authenticated (fun user -> Settings.make ~user ~setUser:setCurrentUser ()) user
          | Login -> Login.make ~setUser:setCurrentUser ()
          | Register -> Register.make ~setUser:setCurrentUser ()
          | CreateArticle -> authenticated (fun _user -> Editor.make ()) user
          | EditArticle slug -> authenticated (fun _user -> Editor.make ~slug ()) user
          | Article _slug -> React.string "Article" (* Article.createElement ~slug ~user ~children:[] () *)
          | Profile username -> Profile.make ~viewMode:(Shape.Profile.Author (username, 10, 0)) ~user ()
          | Favorited username -> Profile.make ~viewMode:(Shape.Profile.Favorited (username, 10, 0)) ~user ()
          | Home -> Home.make ~user ()
          );
          Footer.make ();
        ]
      ()
