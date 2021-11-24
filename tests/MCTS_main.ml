
let city_config = "ch130"
let file_path = "tsp" (* your path to the tsp directory, use the path from root if it doesn't work *)
let city_count, cities = Readertsp.open_tsp ~file_path city_config
let eval = Basetsp.dists cities
let debug_tree = true 
let path  = Monte_Carlo.procede_mcts ~debug_tree Monte_Carlo.Roulette Monte_Carlo.Standard_deviation city_count eval 600. 300000


let () = print_endline "mcts path : "; Basetsp.print_path path; Showtsp.show_solution_and_wait cities path(* Show the computed solution *)

let () =
    let len = Basetsp.path_length eval path in
    let best_len = Basetsp.best_path_length city_config eval in
    Printf.printf "\n%% of error : %.2f %%\n\n" (100. *.float_of_int(len - best_len) /. float_of_int best_len);
    Basetsp.print_best_path city_config