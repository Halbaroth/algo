module InternalMake (A : Intf.Arr) = struct
  type 'a t = 'a data ref

  and 'a data =
    | Arr of 'a A.t
    | Diff of int * 'a * 'a t
    | Invalid

  let assert_arr arr k =
    match !arr with
    | Arr a -> k a
    | _ -> assert false

  module Make (R : sig val reroot : 'a t -> unit end) = struct
    type nonrec 'a t = 'a t

    let make size = ref (Arr (A.make size))

    let of_list lst = ref (Arr (A.of_list lst))

    let length arr =
      R.reroot arr;
      assert_arr arr A.length

    let get arr i =
      R.reroot arr;
      assert_arr arr @@ fun a -> A.get a i

    let set arr i v =
      R.reroot arr;
      assert_arr arr @@ fun a ->
        let old = A.get a i in
        A.set a i v;
        let res = ref (Arr a) in
        arr := Diff (i, old, res);
        res

    let to_list arr =
      R.reroot arr;
      assert_arr arr A.to_list

    let pp pp_elt fmt arr =
      R.reroot arr;
      assert_arr arr (Format.fprintf fmt "%a" (A.pp pp_elt))

    let show pp_elt = Format.asprintf "%a" (pp pp_elt)
  end

  module Persistent = Make (struct
    let rec reroot arr =
      match !arr with
      | Arr _ -> ()
      | Diff (i, v, arr') ->
          reroot arr';
          assert_arr arr' @@ fun a ->
            let old = A.get a i in
            A.set a i v;
            arr := Arr a;
            arr' := Diff (i, old, arr)
      | Invalid -> assert false
  end)

  module SemiPersistent = Make (struct
    let rec reroot arr =
      match !arr with
      | Arr _ -> ()
      | Diff (i, v, arr') ->
          reroot arr';
          assert_arr arr' @@ fun a ->
            A.set a i v;
            arr := Arr a;
            arr' := Invalid
      | Invalid -> failwith "inaccessible semipersistent array"
  end)
end

module Make (A : Intf.Arr) = struct
  module M = InternalMake (A)
  include M.Persistent
end

module MakeSemi (A : Intf.Arr) = struct
  module M = InternalMake (A)
  include M.SemiPersistent
end
