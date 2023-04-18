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

module type Container = sig
  type data
  type t = data ref
  type el

  module V : Vec.S

  val mk_vec : V.t -> data
  val mk_diff : int -> el -> t -> data
  val mk_pop : int -> t -> data
  val mk_push : el list -> t -> data

  val reroot : t -> V.t
end

module M (V : Vec.S) (C : Container with module V = V and type el = V.el) = struct
  let make size = ref (C.mk_vec (V.make size))

  let of_list lst = ref (C.mk_vec (V.of_list lst))

  let length vec = C.reroot vec |> V.length

  let get vec i = V.get (C.reroot vec) i

  let set vec i v =
    let vc = C.reroot vec in
    let old = V.get vc i in
    V.set vc i v;
    let res = ref (C.mk_vec vc) in
    vec := C.mk_diff i old res;
    res

  let pop i vec =
    let vc = C.reroot vec in
    let len = V.length vc in
    let old = ref [] in
    for i = len - 1 downto len - i do
      old := V.get vc i :: !old
    done;
    V.pop i vc;
    let res = ref (C.mk_vec vc) in
    vec := C.mk_push !old res;
    res

  let push vec lst =
    let vc = C.reroot vec in
    V.push vc lst;
    let res = ref (C.mk_vec vc) in
    vec := C.mk_pop (List.length lst) res;
    res

  let to_array vec = C.reroot vec |> V.to_array |> Array.copy
  let to_list vec = C.reroot vec |> V.to_list

  let pp fmt vec =
    C.reroot vec
    |> Format.fprintf fmt "%a" V.pp

  let show = Format.asprintf "%a" pp
end

module Make (V : Vec.S) = struct
  type el = V.el
  type t = data ref

  and data =
    | Vec of V.t
    | Diff of int * V.el * t
    | Pop of int * t
    | Push of V.el list * t

  include M (V) (struct
    type el = V.el
    type nonrec data = data
    type nonrec t = t

    module V = V

    let mk_vec vc = Vec vc
    let mk_diff i v vec = Diff (i, v, vec)
    let mk_pop i vec = Pop (i, vec)
    let mk_push lst vec = Push (lst, vec)

    let rec reroot vec =
      match !vec with
      | Vec vc -> vc
      | Diff (i, v, vec') ->
          let vc = reroot vec' in
          let old = V.get vc i in
          V.set vc i v;
          vec := Vec vc;
          vec' := Diff (i, old, vec);
          vc
      | Pop (i, vec') ->
          let vc = reroot vec' in
          let len = V.length vc in
          let old = ref [] in
          for i = len - 1 downto len - i  do
            old := V.get vc i :: !old
          done;
          V.pop i vc;
          vec := Vec vc;
          vec' := Push (!old, vec);
          vc
      | Push (lst, vec') ->
          let vc = reroot vec' in
          V.push vc lst;
          vec := Vec vc;
          vec' := Pop (List.length lst, vec);
          vc
  end)
end

module MakeSemi (V : Vec.S) = struct
  type el = V.el
  type t = data ref

  and data =
    | Vec of V.t
    | Diff of int * V.el * t
    | Pop of int * t
    | Push of V.el list * t
    | Invalid

  include M (V) (struct
    type el = V.el
    type nonrec data = data
    type nonrec t = t

    module V = V

    let mk_vec vc = Vec vc
    let mk_diff i v vec = Diff (i, v, vec)
    let mk_pop i vec = Pop (i, vec)
    let mk_push lst vec = Push (lst, vec)

    let rec reroot vec =
      match !vec with
      | Vec vc -> vc
      | Diff (i, v, vec') ->
          let vc = reroot vec' in
          V.set vc i v;
          vec := Vec vc;
          vec' := Invalid;
          vc
      | Pop (i, vec') ->
          let vc = reroot vec' in
          V.pop i vc;
          vec := Vec vc;
          vec' := Invalid;
          vc
      | Push (lst, vec') ->
          let vc = reroot vec' in
          V.push vc lst;
          vec := Vec vc;
          vec' := Invalid;
          vc
      | Invalid -> failwith "inaccessible semipersistant vector"
  end)
end
