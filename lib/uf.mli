module type S = sig
  type t
  type el

  val empty : t
  val make : t -> el -> unit
  val find : t -> el -> el
  val union : t -> el -> el -> unit

  include Intf.Printable with type t := t
end

module Make (X : Intf.Comparable) : S with type el = X.t
