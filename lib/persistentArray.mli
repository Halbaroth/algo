module type S = sig
  type t
  type el

  val make : int -> el -> t
  val of_list : el list -> t
  val get : t -> int -> el
  val set : t -> int -> el -> t
  val length : t -> int
  val to_array : t -> el array

  include Intf.Printable with type t := t
end

module Make (X : Intf.Printable) : S with type el = X.t

module MakeSemi (X : Intf.Printable) : S with type el = X.t
