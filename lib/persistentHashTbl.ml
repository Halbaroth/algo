(* module InternalMake (H : Intf.HashTbl) = struct
  type 'a t = 'a data ref

  and 'a data =
    | Tbl of 'a H.t
    | Add of H.key * 'a * 'a t
    | Remove of H.key * 'a t
    | Invalid

  let assert_tbl tbl k =
    match !tbl with
    | Tbl t -> k t
    | _ -> assert false

  module Make (R : sig val reroot : 'a t -> unit end) = struct
    type nonrec 'a t = 'a t

    let length tbl =
      R.reroot tbl;
      assert_tbl tbl H.length

    let lookup tbl key =
      R.reroot tbl;
      assert_tbl tbl @@ fun t -> H.lookup t key

    let add tbl key v =
      R.reroot tbl;
      assert_tbl tbl @@ fun t ->
        try
          let old = H.lookup t key in
          H.add t key v;
          let res = ref (Tbl t) in
          tbl := Add (key, old, res);
          res
        with Not_found -> assert false

    let remove tbl key =
      R.reroot tbl;
      assert_tbl tbl @@ fun t ->
        H.remove t key

    let pp pp_elt fmt tbl =
      R.reroot tbl;
      assert_tbl tbl (H.pp pp_elt fmt)

    let show pp_elt = Format.asprintf "%a" (pp pp_elt)
  end

  module Persistent = Make (struct
    let rec reroot tbl =
      match !tbl with
      | Tbl _ -> ()
      | Invalid -> assert false
  end)
end

module Make (H : Intf.HashTbl) = struct
  module M = InternalMake (H)
  include M.Persistent
end

module MakeSemi (H : Intf.HashTbl) = struct
  module M = InternalMake (H)
  include M.SemiPersistent
end *)
