let rec iter_rev f = function
  | [] -> ()
  | x :: xs ->
      iter_rev f xs;
      f x

let map_in_place f a =
  for i = 0 to Array.length a - 1 do
    a.(i) <- f a.(i)
  done

let mapi_in_place f a =
  for i = 0 to Array.length a - 1 do
    a.(i) <- f i a.(i)
  done

let copy_in_place arr model = mapi_in_place (fun i _ -> model.(i)) arr

let strg x = if x = 0. then "0" else Printf.sprintf "%g" x

