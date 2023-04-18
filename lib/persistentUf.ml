(* module type S = sig
  type t
  type el

  val empty : t
  val make : t -> el -> t
  val find : t -> el -> el
  val union : t -> el -> el -> t

  include Intf.Printable with type t := t
end

module Make (V : PersistantVec.S) = struct
  type el = V.el

  type cell = {
    data : el;
    mutable parent : cell option;
    mutable rank : int;
  }
end *)
