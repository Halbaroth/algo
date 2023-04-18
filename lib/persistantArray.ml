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

module type Container = sig
  type data
  type t = data ref
  type el

  val mk_arr : el array -> data
  val mk_diff : int -> el -> t -> data

  val reroot : t -> el array
end

module M (X : Intf.Printable) (C : Container with type el = X.t) = struct
  let make size i = ref (C.mk_arr (Array.init size (fun _ -> i)))

  let of_list lst = ref (C.mk_arr (Array.of_list lst))

  let length arr = C.reroot arr |> Array.length

  let get arr i = (C.reroot arr).(i)

  let set arr i v =
    let a = C.reroot arr in
    let old = a.(i) in
    a.(i) <- v;
    let res = ref (C.mk_arr a) in
    arr := C.mk_diff i old res;
    res

  let to_array arr = C.reroot arr |> Array.copy

  let pp fmt arr =
    let pp_sep fmt () = Format.fprintf fmt "@," in
    C.reroot arr
    |> Array.to_list
    |> Format.pp_print_list ~pp_sep X.pp fmt

  let show = Format.asprintf "%a" pp
end

module Make (X : Intf.Printable) = struct
  type el = X.t
  type t = data ref

  and data =
    | Arr of X.t array
    | Diff of int * X.t * t

  include M (X) (struct
    type el = X.t
    type nonrec data = data
    type nonrec t = t

    let mk_arr a = Arr a
    let mk_diff i v arr = Diff (i, v, arr)

    let rec reroot arr =
      match !arr with
      | Arr a -> a
      | Diff (i, v, arr') ->
          let a = reroot arr' in
          let old = a.(i) in
          a.(i) <- v;
          arr := Arr a;
          arr' := Diff (i, old, arr);
          a
  end)
end

module MakeSemi (X : Intf.Printable) = struct
  type el = X.t
  type t = data ref

  and data =
    | Arr of X.t array
    | Diff of int * X.t * t
    | Invalid

  include M (X) (struct
    type el = X.t
    type nonrec data = data
    type nonrec t = t

    let mk_arr a = Arr a
    let mk_diff i v arr = Diff (i, v, arr)

    let rec reroot arr =
      match !arr with
      | Arr a -> a
      | Diff (i, v, arr') ->
          let a = reroot arr' in
          a.(i) <- v;
          arr := Arr a;
          arr' := Invalid;
          a
      | Invalid -> failwith "inaccessible semipersistant array"
  end)
end
