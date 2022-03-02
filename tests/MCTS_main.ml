(* run command :
   dune exec ./tests/MCTS_main.exe
*)
let city_config = "TSPLIB/att48"

let city_count, cities = Reader_tsp.open_tsp city_config

let adj_matrix = Base_tsp.get_adj_matrix cities

let max_time = 10.

let max_playout = 100000000

let playout_selection_mode = MCTS.Random

let exploration_mode = MCTS.Min_spanning_tree

let expected_length_mode = MCTS.Average

let optimization_mode = MCTS.Full_Two_opt { max_iter = 100; max_time = 1. }

(* let optimization_mode = MCTS.No_opt *)
let generate_log_file = 2

let debug_tree = true

let optimize_end_path = true

let stop_on_leaf = true

let _, (tour, length), tree =
  Printf.printf "%d city_count\n" city_count;

  MCTS.proceed_mcts ~debug_tree ~city_config ~expected_length_mode
    ~playout_selection_mode ~exploration_mode ~optimization_mode
    ~generate_log_file ~stop_on_leaf ~optimize_end_path city_count adj_matrix
    max_time max_playout

let () =
  Graphics.sound 100 1000;
  Graphics.sound 200 1000;
  (*make some sound when monte carlo is complete *)
  Base_tsp.print_error_ratio tour adj_matrix city_config;
  print_endline "mcts tour : ";
  Base_tsp.print_tour tour;
  Show_tsp.show_solution_and_wait cities tour

(* Show the computed solution *)
let () = Show_tsp.show_best_tour_and_wait city_config
