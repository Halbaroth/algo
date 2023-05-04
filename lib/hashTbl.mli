module type Hashable = sig
  type t

  val equal : t -> t -> bool
  val hash : t -> int
  val pp : t Fmt.t
  val show : t -> string
end

module Make1 (H : Hashable) : Intf.HashTbl with type key = H.t

module Make2 (H : Hashable) : Intf.HashTbl with type key = H.t
