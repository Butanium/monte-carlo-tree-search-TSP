(* run command :
   dune exec ./tests/MCTS_main.exe
*)
let city_config = "TSPLIB/att48"

let city_count, cities = Reader_tsp.open_tsp city_config

let adj_matrix = Base_tsp.get_adj_matrix cities

let max_time = 3600.

let max_simulation = max_int

let simulation_selection_policy = MCTS.Roulette

let exploration_policy = MCTS.Min_spanning_tree 1.

let expected_length_policy = MCTS.Average
(* let optimization_policy = MCTS.Full_Two_opt { max_iter = 100; max_time = 1. } *)

let optimization_policy = MCTS.No_opt

let generate_log_file = 10

let debug_tree = false

let optimize_end_path = false

let stop_on_leaf = false

let close_nodes = true

let name =
  Printf.sprintf "vanilla-test-%s"
  @@ MCTS.str_of_selection_policy simulation_selection_policy

let _, (tour, length), tree =
  Printf.printf "%d city_count\n" city_count;
  MCTS.proceed_mcts ~name ~debug_tree ~city_config ~expected_length_policy ~close_nodes
    ~simulation_selection_policy ~exploration_policy ~optimization_policy
    ~generate_log_file ~stop_on_leaf ~optimize_end_path ~city_count ~adj_matrix
    max_time max_simulation

let () =
  Graphics.sound 100 1000;
  Graphics.sound 200 1000;
  (*make some sound when monte carlo is complete *)
  Base_tsp.print_error_ratio tour adj_matrix city_config;
  print_endline "mcts tour : ";
  Base_tsp.print_tour tour;
  Show_tsp.show_tour_and_wait cities tour

(* Show the computed solution *)
let () = Show_tsp.show_best_tour_and_wait city_config
