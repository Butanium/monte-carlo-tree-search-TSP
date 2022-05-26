let iter_rev f l = List.iter f @@ List.rev l

let map_in_place f a =
  for i = 0 to Array.length a - 1 do
    a.(i) <- f a.(i)
  done

let matrix_iter f m = Array.iter (fun arr -> Array.iter f arr) m

let init_matrix n p f = Array.init n (fun i -> Array.init p (f i))

let mapi_in_place f a =
  for i = 0 to Array.length a - 1 do
    a.(i) <- f i a.(i)
  done

let copy_in_place arr ~model = mapi_in_place (fun i _ -> model.(i)) arr

let find_index arr el =
  let rec aux i =
    if i >= Array.length arr then raise Not_found
    else if arr.(i) = el then i
    else aux (i + 1)
  in
  aux 0

(* avoid storii *)
let mcts_verbose_message =
  "\n\nStarting MCTS, I'll keep informed every minutes :)\n"
  ^ "You can stop the program at anytime by pressing Ctrl+C and it'll return \
     you its current progress \n\n"
  ^ "    ccee88oo\n\
    \  C8O8O8Q8PoOb o8oo\n\
    \ dOB69QO8PdUOpugoO9bD\n\
     CgggbU8OU qOp qOdoUOdcb\n\
    \    6OuU  /p u gcoUodpP\n\
    \      \\\\\\//  /douUP\n\
    \        \\\\\\////\n\
    \         |||/\\\n\
    \         |||\\/\n\
    \         |:)|\n\
    \   .....//||||\\....\n\n"

let list_comp ( <?> ) key = function
  | [] -> None
  | x :: xs ->
      Some
        (List.fold_left
           (fun (acc, acc_score) x ->
             let score = key x in
             if score <?> acc_score then (x, score) else (acc, acc_score))
           (x, key x)
           xs)

let list_min k l = list_comp ( < ) k l

let list_max k l = list_comp ( > ) k l

let interpolate (x1, y1) (x2, y2) x = y1 +. ((y2 -. y1) *. (x -. x1) /. (x2 -. x1))
