open Js_of_ocaml.Js

type 'a js_t = 'a t

type headers_init

type body_init = Unsafe.top

type request_init = Unsafe.top

type signal

(* externals, not defined in jsoo lib yet *)

type bufferSource (* Web IDL, either an arrayBuffer or arrayBufferView *)

type readableStream (* Streams *)

type urlSearchParams (* URL *)

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

let encode_request_method = (* internal *)
  function
  | Get -> "GET"
  | Head -> "HEAD"
  | Post -> "POST"
  | Put -> "PUT"
  | Delete -> "DELETE"
  | Connect -> "CONNECT"
  | Options -> "OPTIONS"
  | Trace -> "TRACE"
  | Patch -> "PATCH"
  | Other method_ -> method_

let decode_request_method = (* internal *)
  function
  | "GET" -> Get
  | "HEAD" -> Head
  | "POST" -> Post
  | "PUT" -> Put
  | "DELETE" -> Delete
  | "CONNECT" -> Connect
  | "OPTIONS" -> Options
  | "TRACE" -> Trace
  | "PATCH" -> Patch
  | method_ -> Other method_

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

let encode_referrer_policy = (* internal *)
  function
  | NoReferrer -> "no-referrer"
  | None -> ""
  | NoReferrerWhenDowngrade -> "no-referrer-when-downgrade"
  | SameOrigin -> "same-origin"
  | Origin -> "origin"
  | StrictOrigin -> "strict-origin"
  | OriginWhenCrossOrigin -> "origin-when-cross-origin"
  | StrictOriginWhenCrossOrigin -> "strict-origin-when-cross-origin"
  | UnsafeUrl -> "unsafe-url"

let decode_referrer_policy = (* internal *)
  function
  | "no-referrer" -> NoReferrer
  | "" -> None
  | "no-referrer-when-downgrade" -> NoReferrerWhenDowngrade
  | "same-origin" -> SameOrigin
  | "origin" -> Origin
  | "strict-origin" -> StrictOrigin
  | "origin-when-cross-origin" -> OriginWhenCrossOrigin
  | "strict-origin-when-cross-origin" -> StrictOriginWhenCrossOrigin
  | "unsafe-url" -> UnsafeUrl
  | e -> raise (Failure ("Unknown referrerPolicy: " ^ e))

type request_type =
  | None (* default? unknown? just empty string in spec *)
  | Audio
  | Font
  | Image
  | Script
  | Style
  | Track
  | Video

let decode_request_type = (* internal *)
  function
  | "audio" -> Audio
  | "" -> None
  | "font" -> Font
  | "image" -> Image
  | "script" -> Script
  | "style" -> Style
  | "track" -> Track
  | "video" -> Video
  | e -> raise (Failure ("Unknown requestType: " ^ e))

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

let decode_request_destination = (* internal *)
  function
  | "document" -> Document
  | "" -> None
  | "embed" -> Embed
  | "font" -> Font
  | "image" -> Image
  | "manifest" -> Manifest
  | "media" -> Media
  | "object" -> Object
  | "report" -> Report
  | "script" -> Script
  | "serviceworker" -> ServiceWorker
  | "sharedworder" -> SharedWorker
  | "style" -> Style
  | "worker" -> Worker
  | "xslt" -> Xslt
  | e -> raise (Failure ("Unknown requestDestination: " ^ e))

type request_mode =
  | Navigate
  | SameOrigin
  | NoCORS
  | CORS

let encode_request_mode = (* internal *)
  function
  | Navigate -> "navigate"
  | SameOrigin -> "same-origin"
  | NoCORS -> "no-cors"
  | CORS -> "cors"

let decode_request_mode = (* internal *)
  function
  | "navigate" -> Navigate
  | "same-origin" -> SameOrigin
  | "no-cors" -> NoCORS
  | "cors" -> CORS
  | e -> raise (Failure ("Unknown requestMode: " ^ e))

type request_credentials =
  | Omit
  | SameOrigin
  | Include

let encode_request_credentials = (* internal *)
  function
  | Omit -> "omit"
  | SameOrigin -> "same-origin"
  | Include -> "include"

let decode_request_credentials = (* internal *)
  function
  | "omit" -> Omit
  | "same-origin" -> SameOrigin
  | "include" -> Include
  | e -> raise (Failure ("Unknown requestCredentials: " ^ e))

type request_cache =
  | Default
  | NoStore
  | Reload
  | NoCache
  | ForceCache
  | OnlyIfCached

let encode_request_cache = (* internal *)
  function
  | Default -> "default"
  | NoStore -> "no-store"
  | Reload -> "reload"
  | NoCache -> "no-cache"
  | ForceCache -> "force-cache"
  | OnlyIfCached -> "only-if-cached"

let decode_request_cache = (* internal *)
  function
  | "default" -> Default
  | "no-store" -> NoStore
  | "reload" -> Reload
  | "no-cache" -> NoCache
  | "force-cache" -> ForceCache
  | "only-if-cached" -> OnlyIfCached
  | e -> raise (Failure ("Unknown requestCache: " ^ e))

type request_redirect =
  | Follow
  | Error
  | Manual

let encode_request_redirect = (* internal *)
  function
  | Follow -> "follow"
  | Error -> "error"
  | Manual -> "manual"

let decode_request_redirect = (* internal *)
  function
  | "follow" -> Follow
  | "error" -> Error
  | "manual" -> Manual
  | e -> raise (Failure ("Unknown requestRedirect: " ^ e))

module Headers_init = struct
  type t = headers_init

  let make : (string * string) list -> headers_init js_t =
   fun arr -> Unsafe.obj (Array.map (fun (key, value) -> key, Unsafe.inject (string value)) (Array.of_list arr))
end

module Headers = struct
  class type t =
    object
      method append : js_string js_t -> js_string js_t -> unit meth

      method delete : js_string js_t -> unit meth

      (* entries - very experimental *)

      method get : js_string js_t -> js_string js_t opt meth

      method has : js_string js_t -> bool js_t meth

      (* keys - very experimental *)

      method set : js_string js_t -> js_string js_t -> unit meth

      (* values - very experimental *)
    end

  let append : name:string -> value:string -> t js_t -> unit =
   fun ~name ~value headers -> headers##append (string name) (string value)

  let delete : name:string -> t js_t -> unit = fun ~name headers -> headers##delete (string name)

  let get : name:string -> t js_t -> string option =
   fun ~name headers -> Option.map to_string (Opt.to_option (headers##get (string name)))

  let has : name:string -> t js_t -> bool = fun ~name headers -> to_bool (headers##has (string name))

  let set : name:string -> value:string -> t js_t -> unit =
   fun ~name ~value headers -> headers##set (string name) (string value)

  let headers = Unsafe.global##._Headers

  let make_withInit : (headers_init js_t -> t js_t) constr = headers

  let make_withHeaders : (t js_t -> t js_t) constr = headers
end

module Body_init = struct
  type t = body_init

  let make : string -> t js_t = fun str -> Unsafe.inject (string str)

  let make_withBlob : Js_of_ocaml.File.blob -> t js_t = Unsafe.inject

  let make_withBufferSource : bufferSource -> t js_t = Unsafe.inject

  let make_withFormData : Js_of_ocaml.Form.formData -> t js_t = Unsafe.inject

  let make_withUrlSearchParams : urlSearchParams -> t js_t = Unsafe.inject
end

module Body = struct
  class type t =
    object
      method body : readableStream js_t readonly_prop

      method bodyUsed : bool js_t readonly_prop

      method arrayBuffer : unit -> Js_of_ocaml.Typed_array.arrayBuffer js_t Promise.t meth

      method blob : unit -> Js_of_ocaml.File.blob js_t Promise.t meth

      method formData : unit -> Js_of_ocaml.Form.formData js_t Promise.t meth

      method json : unit -> 'a js_t Promise.t meth

      method text : unit -> js_string js_t Promise.t meth
    end

  let body : t js_t -> readableStream js_t = fun body -> body##.body

  let bodyUsed : t js_t -> bool js_t = fun body -> body##.bodyUsed

  let arrayBuffer : t js_t -> Js_of_ocaml.Typed_array.arrayBuffer js_t Promise.t = fun body -> body##arrayBuffer ()

  let blob : t js_t -> Js_of_ocaml.File.blob js_t Promise.t = fun body -> body##blob ()

  let formData : t js_t -> Js_of_ocaml.Form.formData js_t Promise.t = fun body -> body##formData ()

  let json : t js_t -> 'a js_t Promise.t = fun body -> body##json ()

  let text : t js_t -> js_string js_t Promise.t = fun body -> body##text ()
end

module RequestInit = struct
  type t = request_init

  let wrap_opt_string : 'a option -> Unsafe.any option = function
    (* internal *)
    | None -> None
    | Some v -> Some (Unsafe.inject (string v))

  let make
    :  ?method_:request_method -> ?headers:headers_init js_t -> ?body:Body_init.t js_t -> ?referrer:string ->
    ?referrerPolicy:referrer_policy -> ?mode:request_mode -> ?credentials:request_credentials -> ?cache:request_cache ->
    ?redirect:request_redirect -> ?integrity:string -> ?keepalive:bool -> ?signal:signal js_t -> unit -> t js_t
    =
   fun ?method_ ?headers ?body ?referrer ?referrerPolicy ?mode ?credentials ?cache ?redirect ?integrity ?keepalive
     ?signal () ->
    let options =
      List.filter_map
        (fun (name, (v : 'a option)) ->
          match v with
          | None -> None
          | Some v -> Some (name, v)
        )
        [
          "method", wrap_opt_string (Option.map encode_request_method method_);
          "headers", Option.map Unsafe.inject headers;
          "body", Option.map Unsafe.inject body;
          "referrer", wrap_opt_string referrer;
          "referrerPolicy", wrap_opt_string (Option.map encode_referrer_policy referrerPolicy);
          "mode", wrap_opt_string (Option.map encode_request_mode mode);
          "credentials", wrap_opt_string (Option.map encode_request_credentials credentials);
          "cache", wrap_opt_string (Option.map encode_request_cache cache);
          "redirect", wrap_opt_string (Option.map encode_request_redirect redirect);
          "integrity", wrap_opt_string integrity;
          "keepalive", Option.map (fun v -> Unsafe.inject (bool v)) keepalive;
          "signal", Option.map Unsafe.inject signal;
        ]
    in
    match options with
    | [] -> Unsafe.inject undefined
    | l -> Unsafe.obj (Array.of_list l)
end

module Request = struct
  class type t =
    object
      inherit Body.t

      method _method : js_string js_t readonly_prop

      method url : js_string js_t readonly_prop

      method headers : Headers.t js_t readonly_prop

      method _type : js_string js_t readonly_prop

      method destination : js_string js_t readonly_prop

      method referrer : js_string js_t readonly_prop

      method referrerPolicy : js_string js_t readonly_prop

      method mode : js_string js_t readonly_prop

      method credentials : js_string js_t readonly_prop

      method cache : js_string js_t readonly_prop

      method redirect : js_string js_t readonly_prop

      method integrity : js_string js_t readonly_prop

      method keepalive : bool js_t readonly_prop

      method signal : signal js_t readonly_prop
    end

  (* inherited from Body, can this be included instead of duped? *)

  let body : t js_t -> readableStream js_t = fun body -> body##.body

  let bodyUsed : t js_t -> bool js_t = fun body -> body##.bodyUsed

  let arrayBuffer : t js_t -> Js_of_ocaml.Typed_array.arrayBuffer js_t Promise.t = fun body -> body##arrayBuffer ()

  let blob : t js_t -> Js_of_ocaml.File.blob js_t Promise.t = fun body -> body##blob ()

  let formData : t js_t -> Js_of_ocaml.Form.formData js_t Promise.t = fun body -> body##formData ()

  let json : t js_t -> 'a js_t Promise.t = fun body -> body##json ()

  let text : t js_t -> js_string js_t Promise.t = fun body -> body##text ()

  (* end inherited from Body *)

  let request_constr = Unsafe.global##._Request

  let make : js_string js_t -> t js_t constr = request_constr

  let make_withInit : js_string js_t -> request_init js_t -> t js_t constr = request_constr

  let make_withRequest : t js_t -> t js_t constr = request_constr

  let make_withRequestInit : t js_t -> request_init -> t js_t constr = request_constr

  let method_ : t js_t -> request_method = fun req -> decode_request_method (to_string req##._method)

  let url : t js_t -> string = fun req -> to_string req##.url

  let headers : t js_t -> Headers.t js_t = fun req -> req##.headers

  let type_ : t js_t -> request_type = fun req -> decode_request_type (to_string req##._type)

  let destination : t js_t -> request_destination = fun req -> decode_request_destination (to_string req##.destination)

  let referrer : t js_t -> string = fun req -> to_string req##.referrer

  let referrer_policy : t js_t -> referrer_policy = fun req -> decode_referrer_policy (to_string req##.referrerPolicy)

  let mode : t js_t -> request_mode = fun req -> decode_request_mode (to_string req##.mode)

  let credentials : t js_t -> request_credentials = fun req -> decode_request_credentials (to_string req##.credentials)

  let cache : t js_t -> request_cache = fun req -> decode_request_cache (to_string req##.cache)

  let redirect : t js_t -> request_redirect = fun req -> decode_request_redirect (to_string req##.redirect)

  let integrity : t js_t -> string = fun req -> to_string req##.integrity

  let keepalive : t js_t -> bool = fun req -> to_bool req##.keepalive

  let signal : t js_t -> signal js_t = fun req -> req##.signal
end

module Response = struct
  class type t =
    object
      inherit Body.t

      method headers : Headers.t js_t readonly_prop

      method ok : bool js_t readonly_prop

      method redirected : bool js_t readonly_prop

      method status : int readonly_prop

      method statusText : js_string js_t readonly_prop

      method _type : js_string js_t readonly_prop

      method url : js_string js_t readonly_prop

      method clone : unit -> t js_t meth
    end

  class type response =
    object
      method error : unit -> t

      method redirect : string -> t

      method redirect_withStatus : string -> int (* enum-ish *) -> t
    end

  let response = Unsafe.global##._Response

  let response_constr : (unit -> t js_t) constr = response

  (* inherited from Body, can this be included instead of duped? *)

  let body : t js_t -> readableStream js_t = fun body -> body##.body

  let bodyUsed : t js_t -> bool js_t = fun body -> body##.bodyUsed

  let arrayBuffer : t js_t -> Js_of_ocaml.Typed_array.arrayBuffer js_t Promise.t = fun body -> body##arrayBuffer ()

  let blob : t js_t -> Js_of_ocaml.File.blob js_t Promise.t = fun body -> body##blob ()

  let formData : t js_t -> Js_of_ocaml.Form.formData js_t Promise.t = fun body -> body##formData ()

  let json : t js_t -> 'a js_t Promise.t = fun body -> body##json ()

  let text : t js_t -> js_string js_t Promise.t = fun body -> body##text ()

  (* end inherited from Body *)

  let headers : t js_t -> Headers.t js_t = fun req -> req##.headers

  let ok : t js_t -> bool = fun req -> to_bool req##.ok

  let redirected : t js_t -> bool = fun req -> to_bool req##.redirected

  let status : t js_t -> int = fun req -> req##.status

  let statusText : t js_t -> string = fun req -> to_string req##.statusText

  let type_ : t js_t -> string = fun req -> to_string req##._type

  let url : t js_t -> string = fun req -> to_string req##.url

  let clone : t js_t -> t js_t = fun req -> req##clone ()
end

let unsafe_fetch = Unsafe.global##.fetch

let fetch : string -> Response.t js_t Promise.t =
 fun resource -> Unsafe.fun_call unsafe_fetch [| Unsafe.inject (string resource) |]

let fetch_withInit : string -> request_init js_t -> Response.t js_t Promise.t =
 fun resource init -> Unsafe.fun_call unsafe_fetch [| Unsafe.inject (string resource); Unsafe.inject init |]

let fetch_withRequest : Request.t -> Response.t js_t Promise.t =
 fun req -> Unsafe.fun_call unsafe_fetch [| Unsafe.inject req |]

let fetch_withRequestInit : Request.t -> request_init js_t -> Response.t js_t Promise.t =
 fun req init -> Unsafe.fun_call unsafe_fetch [| Unsafe.inject req; Unsafe.inject init |]
