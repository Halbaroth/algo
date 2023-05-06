type 'a t = { mutable data : 'a array; mutable size : int; dummy : 'a }

exception OutOfBound of int * int
exception Empty

let make ~dummy cap =
  { data = Array.init cap (fun _ -> dummy); size = 0; dummy }

let of_array ~dummy arr =
  { data = Array.copy arr; size = Array.length arr; dummy }

let of_list ~dummy lst =
  { data = Array.of_list lst; size = List.length lst; dummy }

let to_array { data; size; _ } = Array.sub data 0 size

let to_list { data; size; _ } =
  let rec aux acc = function
    | 0 -> acc
    | i -> aux (data.(i-1) :: acc) (i-1)
  in
  aux [] size

let get vec i =
  if i < 0 || i >= vec.size then
    raise (OutOfBound (i, vec.size))
  else
    Array.unsafe_get vec.data i

let set vec i v =
  if i < 0 || i >= vec.size then
    raise (OutOfBound (i, vec.size))
  else
    Array.unsafe_set vec.data i v

let[@inline_always] length { size; _ } = size

let[@inline_always] capacity { data; _ } = Array.length data

let grow ({ data; size; dummy } as vec) cap =
  if cap > Array.length data then
    let data =
      Array.init cap (fun i -> if i < size then data.(i) else dummy)
    in
    vec.data <- data

let shrink ({ data; size; _ } as vec) cap =
  assert (cap >= size);
  if cap < capacity vec then
    vec.data <- Array.init cap (fun i -> data.(i))

let push ({ size; _ } as vec) v =
  let cap = Array.length vec.data in
  if cap = size then
    grow vec (2 * (max cap 1));
  Array.unsafe_set vec.data size v;
  vec.size <- size + 1

let%test "push1" =
  let vec = make ~dummy:0 50 in
  Int.equal (push vec 1; get vec 0) 1

let%test "push2" =
  let vec = make ~dummy:0 50 in
  for i = 0 to 99 do
    push vec i
  done;
  Int.equal (length vec) 100

let%test "push3" =
  let vec = make ~dummy:0 50 in
  for i = 0 to 99 do
    push vec i
  done;
  Int.equal (get vec 65) 65

let pop ({ size; _ } as vec) =
  if size = 0 then raise Empty
  else
    begin
      if 2 * size < capacity vec then shrink vec size
      else vec.size <- size - 1
    end

let exists ~f { data; size; _ } =
  let exception Exit in
  try
    for i = 0 to size-1 do
      if f (Array.unsafe_get data i) then raise Exit
    done;
    false
  with Exit -> true

let for_all ~f vec = not @@ exists ~f:(fun elt -> not @@ f elt) vec

let iter ~f { data; size; _ } =
  for i = 0 to size-1 do
    f (Array.unsafe_get data i)
  done

let fold_left ~f ~init { data; size; _ } =
  let acc = ref init in
  for i = 0 to size-1 do
    acc := f !acc (Array.unsafe_get data i)
  done;
  !acc

let pp pp_elt fmt { data; size; _ } =
  let pp_sep fmt () = Format.fprintf fmt "@," in
  let lst = Array.(sub data 0 size |> to_list) in
  Format.pp_print_list ~pp_sep pp_elt fmt lst

let show pp_elt = Format.asprintf "%a" (pp pp_elt)
