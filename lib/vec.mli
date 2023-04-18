module type S = sig
  type t
  type el

  exception Out_of_bound of int * int
  exception Empty

  val make : int -> el -> t
  val of_array : el array -> t
  val of_list : el list -> t
  val get : t -> int -> el
  val set : t -> int -> el -> unit
  val push : t -> el -> unit
  val pop : t -> unit
  val length : t -> int
  val capacity : t -> int
  val to_array : t -> el array
  val to_list : t -> el list

  include Intf.Printable with type t := t
end

module Make (X : Intf.Dummyable) : S with type el = X.t
