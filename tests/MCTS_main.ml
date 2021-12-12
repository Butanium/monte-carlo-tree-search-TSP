(*run command : 
dune exec ./tests/MCTS_main.exe 
*)
let city_config = "att48"
let city_count, cities = Reader_tsp.open_tsp city_config
let eval = Base_tsp.dists cities
let max_time = 1800. 
let max_playout = 100000000
let playout_selection_mode = MCTS.Random
let exploration_mode = MCTS.Min_spanning_tree
let expected_length_mode = MCTS.Average

let optimization_mode = MCTS.Two_opt (200, 100, 1.)
(* let optimization_mode = MCTS.No_opt *)
let generate_log_file = true
let debug_tree = false
let optimize_end_path = true
let stop_on_leaf = true
let path, tree = MCTS.proceed_mcts ~debug_tree ~city_config ~expected_length_mode ~playout_selection_mode 
    ~exploration_mode ~optimization_mode ~generate_log_file ~stop_on_leaf ~optimize_end_path
     city_count eval max_time max_playout


let () = Graphics.sound 100 1000; Graphics.sound 200 1000; (*make some sound when monte carlo is complete *)
    print_endline "mcts path : "; Base_tsp.print_path path; Show_tsp.show_solution_and_wait cities path(* Show the computed solution *)

let () =
    Base_tsp.print_error_ratio path eval city_config;
    Show_tsp.show_best_path_and_wait city_config
