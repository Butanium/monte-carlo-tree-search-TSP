let iter_rev f l = List.iter f @@ List.rev l

let map_in_place f a =
  for i = 0 to Array.length a - 1 do
    a.(i) <- f a.(i)
  done

let mapi_in_place f a =
  for i = 0 to Array.length a - 1 do
    a.(i) <- f i a.(i)
  done

let copy_in_place arr model = mapi_in_place (fun i _ -> model.(i)) arr

let find_index arr el =
  let rec aux i =
    if i >= Array.length arr then raise Not_found
    else if arr.(i) = el then i
    else aux (i + 1)
  in
  aux 0
