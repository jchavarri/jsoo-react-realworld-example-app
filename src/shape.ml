type user = {
  email : string;
  username : string;
  bio : string option;
  image : string option;
  token : string;
}
[@@deriving jsobject]

type update_user = {
  email : string;
  username : string;
  bio : string option;
  image : string option;
  password : string option;
}
[@@deriving jsobject]

type update_user_body = { user : update_user } [@@deriving jsobject]

type login_user = {
  email : string;
  password : string;
}
[@@deriving jsobject]

type login_body = { user : login_user } [@@deriving jsobject]

type login_error = string list option [@@deriving jsobject]

type author = {
  username : string;
  bio : string option;
  image : string;
  following : bool;
}
[@@deriving jsobject]

type article = {
  slug : string;
  title : string;
  description : string;
  body : string;
  tagList : string array;
  createdAt : Utils.Timestamp_str.t;
  updatedAt : Utils.Timestamp_str.t;
  favorited : bool;
  favoritesCount : int;
  author : author;
}

type settings = {
  email : string array option;
  bio : string array option;
  image : string array option;
  username : string array option;
  password : string array option;
}
[@@deriving jsobject]

type 'a errors = { errors : 'a } [@@deriving jsobject]
