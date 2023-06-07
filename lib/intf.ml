module type Printable = sig
  type t

  val pp : t Fmt.t
  val show : t -> string
end

module type Comparable = sig
  type t

  val compare : t -> t -> int
  include Printable with type t := t
end

module type Arr = sig
  type 'a t

  val make : int -> 'a t
  val of_list : 'a list -> 'a t
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> unit
  val length : 'a t -> int
  val to_list : 'a t -> 'a list
  val pp : 'a Fmt.t -> 'a t Fmt.t
  val show : 'a Fmt.t -> 'a t -> string
end

module type PersistentArr = sig
  type 'a t

  val make : int -> 'a t
  val of_list : 'a list -> 'a t
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> 'a t
  val length : 'a t -> int
  val to_list : 'a t -> 'a list
  val pp : 'a Fmt.t -> 'a t Fmt.t
  val show : 'a Fmt.t -> 'a t -> string
end

module type Vec = sig
  type 'a t

  val make : dummy:'a -> int -> 'a t
  val of_array : dummy:'a -> 'a array -> 'a t
  val of_list : dummy:'a -> 'a list -> 'a t
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> unit
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> unit
  val length : 'a t -> int
  val capacity : 'a t -> int
  val to_array : 'a t -> 'a array
  val to_list : 'a t -> 'a list
  val iter : f:('a -> unit) -> 'a t -> unit
  val iteri : f:(int -> 'a -> unit) -> 'a t -> unit
  val fold : f:('b -> 'a -> 'b) -> init:'b -> 'a t -> 'b
  val exists : f:('a -> bool) -> 'a t -> bool
  val for_all : f:('a -> bool) -> 'a t -> bool
  val pp : 'a Fmt.t -> 'a t Fmt.t
  val show : 'a Fmt.t -> 'a t -> string
end

module type PersistentVec = sig
  type 'a t

  val make : dummy:'a -> int -> 'a t
  val of_array : dummy:'a -> 'a array -> 'a t
  val of_list : dummy:'a -> 'a list -> 'a t
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> 'a t
  val push : 'a t -> 'a -> 'a t
  val pop : 'a t -> 'a t
  val length : 'a t -> int
  val capacity : 'a t -> int
  val to_array : 'a t -> 'a array
  val to_list : 'a t -> 'a list
  val iteri : f:(int -> 'a -> unit) -> 'a t -> unit
  val iter : f:('a -> unit) -> 'a t -> unit
  val fold_left : f:('b -> 'a -> 'b) -> init:'b -> 'a t -> 'b
  val exists : f:('a -> bool) -> 'a t -> bool
  val for_all : f:('a -> bool) -> 'a t -> bool
  val pp : 'a Fmt.t -> 'a t Fmt.t
  val show : 'a Fmt.t -> 'a t -> string
end

module type HashTbl = sig
  type 'a t
  type key

  val make : int -> 'a t
  val length : 'a t -> int
  val lookup : 'a t -> key -> 'a
  val add : 'a t -> key -> 'a -> unit
  val remove : 'a t -> key -> unit
  val pp : 'a Fmt.t -> 'a t Fmt.t
  val show : 'a Fmt.t -> 'a t -> string
end

module type PersistentHashTbl = sig
  type 'a t
  type key

  val make : int -> 'a t
  val length : 'a t -> int
  val lookup : 'a t -> key -> 'a option
  val add : 'a t -> key -> 'a -> 'a t
  val remove : 'a t -> key -> 'a t
  val pp : 'a Fmt.t -> 'a t Fmt.t
  val show : 'a Fmt.t -> 'a t -> string
end
