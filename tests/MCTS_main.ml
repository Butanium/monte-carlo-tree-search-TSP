
let city_config = "ch130" 
(* let file_path = "tsp" (* your path to the tsp directory, use the path from root if it doesn't work *)
let city_count, cities = Readertsp.open_tsp ~file_path city_config
let eval = Basetsp.dists cities
let path  = Monte_Carlo_Debug.procede_mcts Monte_Carlo_Debug.Roulette Monte_Carlo_Debug.Standard_deviation city_count eval 600. 2000000;;
Showtsp.show_solution cities path;;(* Show the computed solution *)

let len = Basetsp.path_length eval path in
let best_len = Basetsp.best_path_length city_config eval in
    Printf.printf "\n%% of error : %.2f %%" (100. *.float_of_int(len - best_len) /. float_of_int best_len)
 *)

let best_path = Readertsp.open_path city_config

let () = 
    print_string "\n\n best path : ";
    Array.iter (Printf.printf "%d ") best_path