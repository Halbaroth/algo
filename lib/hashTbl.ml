module type Hashable = sig
  type t

  val equal : t -> t -> bool
  val hash : t -> int
  val pp : t Fmt.t
  val show : t -> string
end

module Make1 (H : Hashable) = struct
  module Hashtbl = Hashtbl.Make (H)

  type key = H.t
  type 'a t = 'a Hashtbl.t

  let make = Hashtbl.create
  let length = Hashtbl.length
  let lookup = Hashtbl.find
  let add = Hashtbl.add
  let remove = Hashtbl.remove

  let pp pp_elt fmt tbl =
    let pp_sep fmt () = Format.fprintf fmt "@," in
    let pp_bind fmt (key, v) =
      Format.fprintf fmt "(%a, %a)" H.pp key pp_elt v
    in
    let lst = Hashtbl.to_seq tbl |> List.of_seq in
    Format.pp_print_list ~pp_sep pp_bind fmt lst

  let show pp_elt = Format.asprintf "%a" (pp pp_elt)
end

module Make2 (H : Hashable) = struct
  type key = H.t

  module Bucket = struct
    type 'a t = (key * 'a) list

    let update key v =
      let rec aux acc = function
        | [] -> (key, v) :: acc
        | (k, _) :: tl when H.equal k key ->
            (key, v) :: List.rev_append tl acc
        | hd :: tl -> aux (hd :: acc) tl
      in
      aux []

    let remove key =
      let rec aux acc = function
        | [] -> raise Not_found
        | (k, _) :: tl when H.equal k key ->
            List.rev_append tl acc
        | hd :: tl -> aux (hd :: acc) tl
      in
      aux []
  end

  type 'a t = { mutable data: 'a Bucket.t array; mutable size: int }

  let make i = { data = Array.init (max i 50) (fun _ -> []); size = 0 }

  let hash_modulo key n = (H.hash key) mod n

  let length { data; _ } =
    Array.fold_left (fun acc bucket -> List.length bucket + acc) 0 data

  let[@inline always] capacity { data; _ } = Array.length data

  let lookup ({ data; _ } as tbl) key =
    let hash = hash_modulo key (capacity tbl) in
    List.find (fun (k, _) -> H.equal k key) data.(hash)
    |> snd

  let grow ({ data; _ } as tbl) =
    let n = capacity tbl in
    (* For any integers j, n, we have (j mod 2n) mod n = j mod n. *)
    tbl.data <- Array.init (2 * n) (fun i -> data.(i mod n))

  let shrink ({ data; size } as tbl) =
    if 2 * size < capacity tbl then
      tbl.data <- Array.init size (fun i -> data.(i))

  let add ({ data; size } as tbl) key v =
    if 2 * size > Array.length data then grow tbl;
    let { data; _ } = tbl in
    let hash = hash_modulo key (Array.length data) in
    data.(hash) <- Bucket.update key v data.(hash);
    tbl.size <- size + 1

  let remove ({ data; size } as tbl) key =
    try
      let hash = hash_modulo key (Array.length data) in
      data.(hash) <- Bucket.remove key data.(hash);
      tbl.size <- size - 1;
      shrink tbl
    with Not_found -> ()

  let pp pp_elt fmt { data; _ } =
    let pp_sep fmt () = Format.fprintf fmt "@," in
    let pp_bind fmt (key, v) =
      Format.fprintf fmt "(%a, %a)" H.pp key pp_elt v
    in
    let lst = Array.to_list data |> List.concat in
    Format.pp_print_list ~pp_sep pp_bind fmt lst

  let show pp_elt = Format.asprintf "%a" (pp pp_elt)
end
