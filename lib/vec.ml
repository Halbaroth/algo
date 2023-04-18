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

module Make (X : Intf.Dummyable) = struct
  type t = { mutable data : X.t array; mutable size : int }
  type el = X.t

  exception Out_of_bound of int * int
  exception Empty

  let make size i =
    { data = Array.init size (fun _ -> i); size }

  let to_array { data; size } =
    let copy = Array.init size (fun _ -> X.dummy) in
    Array.blit data 0 copy 0 size;
    copy

  let of_array arr = { data = Array.copy arr; size = Array.length arr }

  let of_list lst = { data = Array.of_list lst; size = List.length lst }

  let to_list { data; size } =
    let rec aux acc = function
      | 0 -> acc
      | i -> aux (data.(i-1) :: acc) (i-1)
    in
    aux [] size

  let get vec i =
    if i < 0 || i >= vec.size then
      raise (Out_of_bound (i, vec.size))
    else
      Array.unsafe_get vec.data i

  let set vec i v =
    if i < 0 || i >= vec.size then
      raise (Out_of_bound (i, vec.size))
    else
      Array.unsafe_set vec.data i v

  let grow ({ data; size } as vec) cap =
    if cap > Array.length data then
      let data =
        Array.init cap (fun i -> if i < size then data.(i) else X.dummy)
      in
      vec.data <- data

  let push ({ data; size } as vec) v =
    let cap = Array.length data in
    if cap = size then
      grow vec (2 * (max cap 1));
    Array.unsafe_set vec.data size v;
    vec.size <- size + 1

  let pop ({ data; size } as vec) =
    if vec.size = 0 then raise Empty
    else
      begin
        Array.unsafe_set data (size - 1) X.dummy;
        vec.size <- size - 1
      end

  let[@inline_always] length { size; _ } = size

  let[@inline_always] capacity { data; _ } = Array.length data

  let pp fmt { data; _ } =
    let pp_sep fmt () = Format.fprintf fmt "@," in
    Format.pp_print_list ~pp_sep X.pp fmt (Array.to_list data)

  let show = Format.asprintf "%a" pp
end
