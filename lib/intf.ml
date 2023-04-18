type 'a printer = Format.formatter -> 'a -> unit

module type Printable = sig
  type t

  val pp : t printer
  val show : t -> string
end

module type Dummyable = sig
  type t

  val dummy : t
  include Printable with type t := t
end

module type Comparable = sig
  type t

  val compare : t -> t -> int
  include Printable with type t := t
end

