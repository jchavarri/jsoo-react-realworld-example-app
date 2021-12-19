type user =
  { email: string
  ; username: string
  ; bio: string option
  ; image: string option
  ; token: string }
[@@deriving jsobject]
