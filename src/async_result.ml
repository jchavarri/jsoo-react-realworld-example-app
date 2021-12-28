type ('a, 'e) t = ('a, 'e) result Async_data.t

let init = Async_data.init

let toBusy = Async_data.toBusy

let completeOk a = Async_data.complete (Ok a)

let completeError e = Async_data.complete (Error e)

let reloadingOk a = Async_data.reloading (Ok a)

let isBusy = Async_data.isBusy

let getOk (v : ('a, 'e) t) : 'a option =
  match v with
  | Init -> None
  | Loading -> None
  | Reloading (Error _) -> None
  | Reloading (Ok a) -> Some a
  | Complete (Error _) -> None
  | Complete (Ok a) -> Some a

let map fn (v : ('a, 'e) t) : ('b, 'e) t =
  match v with
  | Init -> Init
  | Loading -> Loading
  | Reloading (Ok a) -> reloadingOk (fn a)
  | Reloading (Error _) as r -> r
  | Complete (Ok a) -> completeOk (fn a)
  | Complete (Error _) as r -> r

let toIdle = Async_data.toIdle

let debug = Async_data.debug
