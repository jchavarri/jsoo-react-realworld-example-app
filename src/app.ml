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
          | Register -> React.string "Register" (* Register.createElement ~setUser:setCurrentUser ~children:[] () *)
          | CreateArticle ->
            React.string "CreateArticle"
            (* authenticated
                   (fun _user -> Editor.createElement ~children:[] ())
                   user *)
          | EditArticle _slug ->
            React.string "EditArticle"
            (* authenticated
                   (fun _user -> Editor.createElement ~slug ~children:[] ())
                   user *)
          | Article _slug -> React.string "Article" (* Article.createElement ~slug ~user ~children:[] () *)
          | Profile _username ->
            React.string "Profile"
            (* Profile.createElement
                   ~viewMode:(Shape.Profile.Author (username, 10, 0))
                   ~user ~children:[] () *)
          | Favorited _username ->
            React.string "Favorited"
            (* Profile.createElement
                   ~viewMode:(Shape.Profile.Favorited (username, 10, 0))
                   ~user ~children:[] () *)
          | Home -> React.string "Home"
          )
          (* Home.createElement ~user ~children:[] () ) *);
          Footer.make ();
        ]
      ()
