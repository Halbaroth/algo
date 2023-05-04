(* open Algo

module H1 = HashTbl.Make1 (struct
  type t = string

  let equal = String.equal
  let hash = Hashtbl.hash
  let pp fmt = Format.fprintf fmt "%s"
  let show = Format.sprintf "%s"
end)

module H2 = HashTbl.Make2 (struct
  type t = string

  let equal = String.equal
  let hash = Hashtbl.hash
  let pp fmt = Format.fprintf fmt "%s"
  let show = Format.sprintf "%s"
end)

let generate_test ~module_name (module H : Intf.HashTbl with type key = string) =
  let open Core_bench in
  let tbl = H.make 10 in
  let test_add () =
    for i = 0 to 100_000 do
      H.add tbl (Format.sprintf "key_%i" i) i
    done
  in
  let test_lookup () =
    for i = 0 to 100_000 do
      ignore (H.lookup tbl (Format.sprintf "key_%i" i))
    done
  in
  let test_remove () =
    for i = 0 to 100_000 do
      H.remove tbl (Format.sprintf "key_%i" i)
    done
  in
  let name s = Format.sprintf "%s <%s>" module_name s in
  [
    Bench.Test.create ~name:(name "add") test_add;
    Bench.Test.create ~name:(name "lookup") test_lookup;
    Bench.Test.create ~name:(name "remove") test_remove;
  ] *)

let () = ()
  (* let open! Core_unix in
  let open Core_bench in
  Command_unix.run (Bench.make_command (
    generate_test ~module_name:"Stdlib" (module H1) @
    generate_test ~module_name:"Naive"  (module H2)
  )) *)
