type 'a t =
  | Init
  | Loading
  | Reloading of 'a
  | Complete of 'a

let init = Init

let reloading v = Reloading v

let isBusy v =
  match v with
  | Init -> false
  | Loading -> true
  | Reloading _ -> true
  | Complete _ -> false

let isComplete v =
  match v with
  | Init -> false
  | Loading -> false
  | Reloading _ -> false
  | Complete _ -> true

let toBusy v =
  match v with
  | Init -> Loading
  | Loading as a -> a
  | Reloading _ as a -> a
  | Complete a -> Reloading a

let complete v = Complete v

let getValue v =
  match v with
  | Init -> None
  | Loading -> None
  | Reloading a -> Some a
  | Complete a -> Some a

let map fn v =
  match v with
  | Init -> Init
  | Loading -> Loading
  | Reloading a -> Reloading (fn a)
  | Complete a -> Complete (fn a)

let tapComplete fn v =
  match v with
  | Init -> v
  | Loading -> v
  | Reloading _ -> v
  | Complete a ->
    fn a;
    v

let toIdle v =
  match v with
  | Init as a -> a
  | Loading -> Init
  | Reloading a -> Complete a
  | Complete _ as a -> a

let debug v =
  match v with
  | Init -> "Init"
  | Loading -> "Loading"
  | Reloading _ -> "Reloading(_)"
  | Complete _ -> "Complete(_)"
