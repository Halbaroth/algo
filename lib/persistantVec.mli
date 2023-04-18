module type S = sig
  type t
  type el

  val make : int -> t
  val of_list : el list -> t
  val get : t -> int -> el
  val set : t -> int -> el -> t
  val push : t -> el list -> t
  val pop : int -> t -> t
  val length : t -> int
  val to_array : t -> el array
  val to_list : t -> el list

  include Intf.Printable with type t := t
end

module Make (V : Vec.S) : S with type el = V.el

module MakeSemi (V : Vec.S) : S with type el = V.el
