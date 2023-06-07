type 'a t = { mutable data : 'a array; mutable size : int; dummy : 'a }

let[@inline always] length { size; _ } = size

let[@inline always] capacity { data; _ } = Array.length data

let make ~dummy cap =
  let cap = max 10 cap in
  assert (cap < Sys.max_array_length);
  { data = Array.init cap (fun _ -> dummy); size = 0; dummy }

let of_array ~dummy arr =
  { data = Array.copy arr; size = Array.length arr; dummy }

let of_list ~dummy lst =
  { data = Array.of_list lst; size = List.length lst; dummy }

let to_array { data; size; _ } = Array.sub data 0 size

let get { data; size; _ } i =
  if i < 0 || i >= size then invalid_arg "get"
  else
    Array.unsafe_get data i

let set { data; size; _ } i v =
  if i < 0 || i >= size then invalid_arg "set"
  else
    Array.unsafe_set data i v

let grow_to ({ data; size; dummy } as vec) cap =
  assert (cap < Sys.max_array_length);
  if cap > Array.length data then
    let data =
      Array.init cap (fun i ->
        if i < size then
          Array.unsafe_get data i
        else
          dummy)
    in
    vec.data <- data

let shrink ({ data; size; _ } as vec) cap =
  assert (cap >= size);
  if cap < capacity vec then
    vec.data <- Array.init cap (fun i -> data.(i))

let push ({ size; _ } as vec) v =
  let cap = Array.length vec.data in
  if cap = size then
    grow_to vec (2 * (max cap 1));
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
  if size <= 0 then invalid_arg "pop"
  else
    begin
      if 2 * size < capacity vec then shrink vec size
      else vec.size <- size - 1
    end

let iteri ~f { data; size; dummy } =
  for i = 0 to size-1 do
    let elt = Array.unsafe_get data i in
    if not (elt == dummy) then
      f i (elt)
  done

let iter ~f vec = iteri ~f:(fun _ elt -> f elt) vec

exception Terminate

let exists ~f vec =
  try
    iter vec ~f:(fun elt ->
      if f elt then raise Terminate);
    false
  with Terminate -> true

let for_all ~f vec = not @@ exists ~f:(fun elt -> not @@ f elt) vec

let fold ~f ~init vec =
  let acc = ref init in
  iter vec ~f:(fun elt -> acc := f !acc elt);
  !acc

let to_list { data; size; _ } = Array.(sub data 0 size |> to_list)

let pp pp_elt fmt vec =
  let pp_sep fmt () = Format.fprintf fmt "@," in
  Format.pp_print_list ~pp_sep pp_elt fmt (to_list vec)

let show pp_elt = Format.asprintf "%a" (pp pp_elt)
