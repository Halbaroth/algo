module type S = sig
  type t
  type el

  val empty : t
  val make : t -> el -> unit
  val find : t -> el -> el
  val union : t -> el -> el -> unit

  include Intf.Printable with type t := t
end

module Make (X : Intf.Comparable) = struct
  type el = X.t

  type cell = {
    data : el;
    mutable parent : cell option;
    mutable rank : int;
  }

  type t = cell list ref

  let empty : t = ref []

  let make env v =
    match List.find_opt (fun { data; _ } -> data = v)  !env with
    | Some _ -> ()
    | None -> env := { data = v; parent = None; rank = 0 } :: !env

  let _find env v =
    let rec aux cell =
      match cell.parent with
      | Some parent ->
          let repr = aux parent in
          cell.parent <- Some repr;
          repr
      | None -> cell
    in
    match List.find_opt (fun { data; _ } -> data = v) !env with
    | Some cell -> aux cell
    | None -> { data = v; parent = None; rank = 0 }

  let find env v = (_find env v).data

  let union env v1 v2 =
    let repr1 = _find env v1 in
    let repr2 = _find env v2 in
    if X.compare v1 v2 <> 0 then
      if repr1.rank < repr2.rank then
        repr1.parent <- Some repr2
      else
        begin
          repr2.parent <- Some repr1;
          if repr1.rank = repr2.rank then
            repr1.rank <- repr1.rank + 1
        end

  let pp_parent fmt = function
    | Some parent -> Format.fprintf fmt "%a" X.pp parent.data
    | None -> Format.fprintf fmt "None"

  let pp_cell fmt cell =
    Format.fprintf fmt "{ data = %a; parent = %a; rank = %i }" X.pp cell.data
      pp_parent cell.parent cell.rank

  let pp fmt env =
    let pp_sep fmt () = Format.fprintf fmt "@," in
    Format.pp_print_list ~pp_sep pp_cell fmt !env

  let show = Format.asprintf "%a" pp
end
