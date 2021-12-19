open Js_of_ocaml

type headers_init

type body_init

type request_init

(* externals, not defined in jsoo lib yet *)

type bufferSource (* Web IDL, either an arrayBuffer or arrayBufferView *)

type readableStream (* Streams *)

type urlSearchParams (* URL *)

type signal

type request_method =
  | Get
  | Head
  | Post
  | Put
  | Delete
  | Connect
  | Options
  | Trace
  | Patch
  | Other of string

type referrer_policy =
  | None
  | NoReferrer
  | NoReferrerWhenDowngrade
  | SameOrigin
  | Origin
  | StrictOrigin
  | OriginWhenCrossOrigin
  | StrictOriginWhenCrossOrigin
  | UnsafeUrl

type request_type =
  | None (* default? unknown? just empty string in spec *)
  | Audio
  | Font
  | Image
  | Script
  | Style
  | Track
  | Video

type request_destination =
  | None (* default? unknown? just empty string in spec *)
  | Document
  | Embed
  | Font
  | Image
  | Manifest
  | Media
  | Object
  | Report
  | Script
  | ServiceWorker
  | SharedWorker
  | Style
  | Worker
  | Xslt

type request_mode = Navigate | SameOrigin | NoCORS | CORS

type request_credentials = Omit | SameOrigin | Include

type request_cache =
  | Default
  | NoStore
  | Reload
  | NoCache
  | ForceCache
  | OnlyIfCached

type request_redirect = Follow | Error | Manual

module Headers_init : sig
  type t = headers_init

  val make : (string * string) list -> headers_init Js.t
end

module Headers : sig
  class type t =
    object
      method append : Js.js_string Js.t -> Js.js_string Js.t -> unit Js.meth

      method delete : Js.js_string Js.t -> unit Js.meth

      method get : Js.js_string Js.t -> Js.js_string Js.t Js.opt Js.meth

      method has : Js.js_string Js.t -> bool Js.t Js.meth

      method set : Js.js_string Js.t -> Js.js_string Js.t -> unit Js.meth
    end

  val append : name:string -> value:string -> t Js.t -> unit

  val delete : name:string -> t Js.t -> unit

  val get : name:string -> t Js.t -> string option

  val has : name:string -> t Js.t -> bool

  val set : name:string -> value:string -> t Js.t -> unit

  val make_withInit : (Headers_init.t Js.t -> t Js.t) Js.constr

  val make_withHeaders : (t Js.t -> t Js.t) Js.constr
end

module Body_init : sig
  type t = body_init

  val make : string -> t Js.t

  val make_withBlob : File.blob -> t Js.t

  val make_withBufferSource : bufferSource -> t Js.t

  val make_withFormData : Form.formData -> t Js.t

  val make_withUrlSearchParams : urlSearchParams -> t Js.t
end

module Body : sig
  class type t =
    object
      method arrayBuffer :
        unit -> Typed_array.arrayBuffer Js.t Promise.t Js.meth

      method blob : unit -> File.blob Js.t Promise.t Js.meth

      method body : readableStream Js.t Js.readonly_prop

      method bodyUsed : bool Js.t Js.readonly_prop

      method formData : unit -> Form.formData Js.t Promise.t Js.meth

      method json : unit -> 'a Js.t Promise.t Js.meth

      method text : unit -> Js.js_string Js.t Promise.t Js.meth
    end

  val body : t Js.t -> readableStream Js.t

  val bodyUsed : t Js.t -> bool Js.t

  val arrayBuffer : t Js.t -> Typed_array.arrayBuffer Js.t Promise.t

  val blob : t Js.t -> File.blob Js.t Promise.t

  val formData : t Js.t -> Form.formData Js.t Promise.t

  val json : t Js.t -> 'a Js.t Promise.t

  val text : t Js.t -> Js.js_string Js.t Promise.t
end

module RequestInit : sig
  type t = request_init

  val make :
       ?method_:request_method
    -> ?headers:headers_init Js.t
    -> ?body:body_init Js.t
    -> ?referrer:string
    -> ?referrerPolicy:referrer_policy
    -> ?mode:request_mode
    -> ?credentials:request_credentials
    -> ?cache:request_cache
    -> ?redirect:request_redirect
    -> ?integrity:string
    -> ?keepalive:bool
    -> ?signal:signal Js.t
    -> unit
    -> t Js.t
end

module Request : sig
  class type t =
    object
      method _method : Js.js_string Js.t Js.readonly_prop

      method _type : Js.js_string Js.t Js.readonly_prop

      method arrayBuffer :
        unit -> Js_of_ocaml.Typed_array.arrayBuffer Js.t Promise.t Js.meth

      method blob : unit -> Js_of_ocaml.File.blob Js.t Promise.t Js.meth

      method body : readableStream Js.t Js.readonly_prop

      method bodyUsed : bool Js.t Js.readonly_prop

      method cache : Js.js_string Js.t Js.readonly_prop

      method credentials : Js.js_string Js.t Js.readonly_prop

      method destination : Js.js_string Js.t Js.readonly_prop

      method formData : unit -> Js_of_ocaml.Form.formData Js.t Promise.t Js.meth

      method headers : Headers.t Js.t Js.readonly_prop

      method integrity : Js.js_string Js.t Js.readonly_prop

      method json : unit -> 'a Js.t Promise.t Js.meth

      method keepalive : bool Js.t Js.readonly_prop

      method mode : Js.js_string Js.t Js.readonly_prop

      method redirect : Js.js_string Js.t Js.readonly_prop

      method referrer : Js.js_string Js.t Js.readonly_prop

      method referrerPolicy : Js.js_string Js.t Js.readonly_prop

      method signal : signal Js.t Js.readonly_prop

      method text : unit -> Js.js_string Js.t Promise.t Js.meth

      method url : Js.js_string Js.t Js.readonly_prop
    end

  val body : t Js.t -> readableStream Js.t

  val bodyUsed : t Js.t -> bool Js.t

  val arrayBuffer : t Js.t -> Js_of_ocaml.Typed_array.arrayBuffer Js.t Promise.t

  val blob : t Js.t -> Js_of_ocaml.File.blob Js.t Promise.t

  val formData : t Js.t -> Js_of_ocaml.Form.formData Js.t Promise.t

  val json : t Js.t -> 'a Js.t Promise.t

  val text : t Js.t -> Js.js_string Js.t Promise.t

  val request_constr : 'res

  val make : Js.js_string Js.t -> t Js.t Js.constr

  val make_withInit : Js.js_string Js.t -> request_init Js.t -> t Js.t Js.constr

  val make_withRequest : t Js.t -> t Js.t Js.constr

  val make_withRequestInit : t Js.t -> request_init -> t Js.t Js.constr

  val method_ : t Js.t -> request_method

  val url : t Js.t -> string

  val headers : t Js.t -> Headers.t Js.t

  val type_ : t Js.t -> request_type

  val destination : t Js.t -> request_destination

  val referrer : t Js.t -> string

  val referrer_policy : t Js.t -> referrer_policy

  val mode : t Js.t -> request_mode

  val credentials : t Js.t -> request_credentials

  val cache : t Js.t -> request_cache

  val redirect : t Js.t -> request_redirect

  val integrity : t Js.t -> string

  val keepalive : t Js.t -> bool

  val signal : t Js.t -> signal Js.t
end

module Response : sig
  class type t =
    object
      method _type : Js.js_string Js.t Js.readonly_prop

      method arrayBuffer :
        unit -> Typed_array.arrayBuffer Js.t Promise.t Js.meth

      method blob : unit -> File.blob Js.t Promise.t Js.meth

      method body : readableStream Js.t Js.readonly_prop

      method bodyUsed : bool Js.t Js.readonly_prop

      method clone : unit -> t Js.t Js.meth

      method formData : unit -> Form.formData Js.t Promise.t Js.meth

      method headers : Headers.t Js.t Js.readonly_prop

      method json : unit -> 'a Js.t Promise.t Js.meth

      method ok : bool Js.t Js.readonly_prop

      method redirected : bool Js.t Js.readonly_prop

      method status : int Js.readonly_prop

      method statusText : Js.js_string Js.t Js.readonly_prop

      method text : unit -> Js.js_string Js.t Promise.t Js.meth

      method url : Js.js_string Js.t Js.readonly_prop
    end

  class type response =
    object
      method error : unit -> t

      method redirect : string -> t

      method redirect_withStatus : string -> int -> t
    end

  val response_constr : (unit -> t Js.t) Js.constr

  val body : t Js.t -> readableStream Js.t

  val bodyUsed : t Js.t -> bool Js.t

  val arrayBuffer : t Js.t -> Typed_array.arrayBuffer Js.t Promise.t

  val blob : t Js.t -> File.blob Js.t Promise.t

  val formData : t Js.t -> Form.formData Js.t Promise.t

  val json : t Js.t -> 'a Js.t Promise.t

  val text : t Js.t -> Js.js_string Js.t Promise.t

  val headers : t Js.t -> Headers.t Js.t

  val ok : t Js.t -> bool

  val redirected : t Js.t -> bool

  val status : t Js.t -> int

  val statusText : t Js.t -> string

  val type_ : t Js.t -> string

  val url : t Js.t -> string

  val clone : t Js.t -> t Js.t
end

val fetch : string -> Response.t Js.t Promise.t

val fetch_withInit : string -> request_init Js.t -> Response.t Js.t Promise.t

val fetch_withRequest : Request.t -> Response.t Js.t Promise.t

val fetch_withRequestInit :
  Request.t -> request_init Js.t -> Response.t Js.t Promise.t
