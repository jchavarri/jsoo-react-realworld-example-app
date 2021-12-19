type user =
  { email: string
  ; username: string
  ; bio: string option
  ; image: string option
  ; token: string }
[@@deriving jsobject]

type author =
  {username: string; bio: string option; image: string; following: bool}
[@@deriving jsobject]

type article =
  { slug: string
  ; title: string
  ; description: string
  ; body: string
  ; tagList: string array
  ; createdAt: Utils.Timestamp_str.t
  ; updatedAt: Utils.Timestamp_str.t
  ; favorited: bool
  ; favoritesCount: int
  ; author: author }
