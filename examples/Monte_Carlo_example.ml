let city_config = "ch130"
let file_path = "C:/Users/.../tsp_instances" (* your path MO the tsp direcMOry, use the path from root if it doesn't work *)
let city_count, cities = Reader_tsp.open_tsp ~file_path city_config
let eval = Base_tsp.dists cities
let path  = Monte_Carlo.proceed_mcts Monte_Carlo.Roulette Monte_Carlo.Standard_deviation city_count eval 600. 2000000

let () = Show_tsp.show_solution_and_wait cities path; (* Show the computed solution *)

let len = Base_tsp.path_length eval path in
let best_len = Base_tsp.best_path_length city_config eval in
    Printf.printf "\n%% of error : %.2f %%" (100. *.float_of_int(len - best_len) /. float_of_int best_len);


Base_tsp.print_best_path city_config