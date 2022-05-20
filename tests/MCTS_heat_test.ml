(* run command :
   dune exec ./tests/MCTS_main.exe
*)
let city_config = "TSPLIB/att48"

let city_count, cities = Reader_tsp.open_tsp city_config

let adj_matrix = Base_tsp.get_adj_matrix cities

let max_time = 10.

let max_playout = 100000000

let exploration_policy = MCTS_Heat.Min_spanning_tree 1.

let expected_length_policy = MCTS_Heat.Average

let optimization_policy = MCTS_Heat.Two_opt { max_iter = 100; max_time = 1. }

(* let optimization_policy = MCTS_Heat.No_opt *)
let generate_log_file = -1

let debug_tree = true

let optimize_end_path = true

let stop_on_leaf = true

let _, (tour, length), tree =
  Printf.printf "%d city_count\n" city_count;

  MCTS_Heat.proceed_mcts ~city_config ~expected_length_policy
    ~exploration_policy ~optimization_policy ~generate_log_file
    ~optimize_end_path city_count adj_matrix max_time max_playout

let () =
  Graphics.sound 100 1000;
  Graphics.sound 200 1000;
  (*make some sound when monte carlo is complete *)
  Printexc.record_backtrace true;
  Base_tsp.print_error_ratio tour adj_matrix city_config;
  print_endline "mcts tour : ";
  Base_tsp.print_tour tour;
  Show_tsp.show_solution_and_wait cities tour

(* Show the computed solution *)
let () = Show_tsp.show_best_tour_and_wait city_config
