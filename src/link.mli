type location'

type onClickAction =
  | Location of location'
  | CustomFn of (unit -> unit)

val customFn : (unit -> unit) -> onClickAction

val location : location' -> onClickAction

val home : location'

val settings : location'

val register : location'

val login : location'

val createArticle : location'

val editArticle : slug:string -> location'

val article : slug:string -> location'

val profile : username:string -> location'

val favorited : username:string -> location'

val push : location' -> unit

val availableIf : bool -> onClickAction -> onClickAction

val make
  :  ?className:string ->
  ?style:React.Dom.block ->
  onClick:onClickAction ->
  children:React.element list ->
  ?key:string ->
  unit ->
  React.element

module Button : sig
  val make
    :  ?className:string ->
    ?style:React.Dom.block ->
    onClick:onClickAction ->
    ?disabled:bool ->
    children:React.element list ->
    ?key:string ->
    unit ->
    React.element
end
