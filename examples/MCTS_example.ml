(*run command : dune exec ./examples/MCTS_example.exe *)
let city_config = "att48"
let file_path = "tsp_instances" (* the path to the tsp_instance directory, use the path from root if it doesn't work *)
let city_count, cities = Reader_tsp.open_tsp ~file_path city_config
let eval = Base_tsp.dists cities
let debug_tree = true 
let generate_log_file = false
let max_time = 180. (* 3 minute run, can be longer like 30 minutes for good results *)
let max_playout = 100000000 
let playout_selection_mode = MCTS.Roulette
let exploration_mode = MCTS.Min_spanning_tree
let path, tree  = MCTS.proceed_mcts ~debug_tree ~generate_log_file ~city_config ~playout_selection_mode ~exploration_mode
    city_count eval max_time max_playout


let () = Graphics.sound 100 2000; Graphics.sound 200 1000; (*make some sound when monte carlo is complete *)
    print_endline "mcts path : "; Base_tsp.print_path path; 
    Show_tsp.show_solution_and_wait cities path(* Show the computed solution *)

let () =
    Base_tsp.print_error_ratio path eval city_config
