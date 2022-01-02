module Profile = struct
  type username = string

  type limit = int

  type offset = int

  type viewMode =
    | Author of username * limit * offset
    | Favorited of username * limit * offset
end

module Feed_type = struct
  type tag = string

  type limit = int

  type offset = int

  type t =
    | Tag of tag * limit * offset
    | Global of limit * offset
    | Personal of limit * offset
end

type user = {
  email : string;
  username : string;
  bio : string option;
  image : string option;
  token : string;
}
[@@deriving jsobject]

type user_response = { user : user } [@@deriving jsobject]

type update_user = {
  email : string;
  username : string;
  bio : string option;
  image : string option;
  password : string option;
}
[@@deriving jsobject]

type 'a user_body = { user : 'a } [@@deriving jsobject]

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

type article_response = {
  slug : string;
  title : string;
  description : string;
  body : string;
  tagList : string list;
  createdAt : Utils.Timestamp_str.t;
  updatedAt : Utils.Timestamp_str.t;
  favorited : bool;
  favoritesCount : int;
  author : author;
}
[@@deriving jsobject]

type articles = {
  articles : article_response array;
  articlesCount : int;
}
[@@deriving jsobject]

type create_article = {
  title : string;
  description : string;
  body : string;
  tagList : string list;
}
[@@deriving jsobject]

type 'a article = { article : 'a } [@@deriving jsobject]

type settings = {
  email : string array option;
  bio : string array option;
  image : string array option;
  username : string array option;
  password : string array option;
}
[@@deriving jsobject]

type 'a errors = { errors : 'a } [@@deriving jsobject]

type register_error = {
  email : string array option;
  password : string array option;
  username : string array option;
}
[@@deriving jsobject]

type register = {
  email : string;
  password : string;
  username : string;
}
[@@deriving jsobject]

type editor_error = {
  title : string array option;
  body : string array option;
  description : string array option;
}
[@@deriving jsobject]

type tags = { tags : string array } [@@deriving jsobject]

type profile = { author : author } [@@deriving jsobject]
