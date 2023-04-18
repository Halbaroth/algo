module Make (V : Intf.Vec) = struct
  type 'a t = 'a data ref

  and 'a data =
    | Vec of 'a V.t
    | Diff of int * 'a * 'a t
    | Pop of 'a t
    | Push of 'a * 'a t
    | Invalid

  let assert_vec vec k =
    match !vec with
    | Vec vc -> k vc
    | _ -> assert false

  module Make (R : sig val reroot : 'a t -> unit end) = struct
    type nonrec 'a t = 'a t

    let make ~dummy size = ref (Vec (V.make ~dummy size))

    let of_array ~dummy arr = ref (Vec (V.of_array ~dummy arr))
    let of_list ~dummy lst = ref (Vec (V.of_list ~dummy lst))

    let length vec =
      R.reroot vec;
      assert_vec vec V.length

    let get vec i =
      R.reroot vec;
      assert_vec vec @@ fun vc -> V.get vc i

    let set vec i v =
      R.reroot vec;
      assert_vec vec @@ fun vc ->
        let old = V.get vc i in
        V.set vc i v;
        let res = ref (Vec vc) in
        vec := Diff (i, old, res);
        res

    let pop vec =
      R.reroot vec;
      assert_vec vec @@ fun vc ->
        let old = V.get vc (V.length vc - 1) in
        V.pop vc;
        let res = ref (Vec vc) in
        vec := Push (old, res);
        res

    let push vec v =
      R.reroot vec;
      assert_vec vec @@ fun vc ->
        V.push vc v;
        let res = ref (Vec vc) in
        vec := Pop res;
        res

    let capacity vec =
      R.reroot vec;
      assert_vec vec V.capacity

    let to_array vec =
      R.reroot vec;
      assert_vec vec @@ fun vc -> V.to_array vc |> Array.copy

    let to_list vec =
      R.reroot vec;
      assert_vec vec V.to_list

    let pp pp_elt fmt vec =
      R.reroot vec;
      assert_vec vec (Format.fprintf fmt "%a" (V.pp pp_elt))

    let show pp_elt = Format.asprintf "%a" (pp pp_elt)
  end

  module P = Make (struct
    let rec reroot vec =
      match !vec with
      | Vec _ -> ()
      | Diff (i, v, vec') ->
          reroot vec';
          assert_vec vec' @@ fun vc ->
            let old = V.get vc i in
            V.set vc i v;
            vec := Vec vc;
            vec' := Diff (i, old, vec)
      | Pop vec' ->
          reroot vec';
          assert_vec vec'@@ fun vc ->
            let old = V.get vc (V.length vc - 1) in
            V.pop vc;
            vec := Vec vc;
            vec' := Push (old, vec)
      | Push (v, vec') ->
          reroot vec';
          assert_vec vec' @@ fun vc ->
            V.push vc v;
            vec := Vec vc;
            vec' := Pop vec
      | Invalid -> assert false
  end)

  module SP = Make (struct
    let rec reroot vec =
      match !vec with
      | Vec _ -> ()
      | Diff (i, v, vec') ->
          reroot vec';
          assert_vec vec' @@ fun vc ->
            V.set vc i v;
            vec := Vec vc;
            vec' := Invalid
      | Pop vec' ->
          reroot vec';
          assert_vec vec' @@ fun vc ->
            V.pop vc;
            vec := Vec vc;
            vec' := Invalid
      | Push (v, vec') ->
          reroot vec';
          assert_vec vec' @@ fun vc ->
            V.push vc v;
            vec := Vec vc;
            vec' := Invalid
      | Invalid -> failwith "inaccessible semipersistant vector"
  end)
end
