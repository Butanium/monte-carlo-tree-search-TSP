(*run command : dune exec ./examples/MCTS_example.exe *)
let city_config = "att48"

let file_path = "tsp_instances"
(* the tour to the tsp_instance directory, use the tour from root if it doesn't work *)

let city_count, cities = Reader_tsp.open_tsp ~file_path city_config

let adj_matrix = Base_tsp.get_adj_matrix cities

let debug_tree = true

let generate_log_file = 0

let max_time = 180.
(* 3 minute run, can be longer like 30 minutes for good results *)

let max_simulation = 100000000

let simulation_selection_policy = MCTS.Roulette

let exploration_policy = MCTS.Min_spanning_tree 1.

let (tour, length), (opt_path, opt_length), root =
  MCTS.proceed_mcts ~debug_tree ~generate_log_file ~city_config
    ~simulation_selection_policy ~exploration_policy ~city_count ~adj_matrix max_time
    max_simulation

let () =
  Graphics.sound 100 2000;
  Graphics.sound 200 1000;
  (*make some sound when monte carlo is complete *)
  print_endline "mcts tour : ";
  Base_tsp.print_tour tour;
  Show_tsp.show_tour_and_wait cities tour
(* Show the computed solution *)

let () = Base_tsp.print_error_ratio tour adj_matrix city_config
