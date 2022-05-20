let iter_rev f l = List.iter f @@ List.rev l

let map_in_place f a =
  for i = 0 to Array.length a - 1 do
    a.(i) <- f a.(i)
  done

let init_matrix n p f = 
  Array.init n (fun i -> Array.init p (f i))

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
