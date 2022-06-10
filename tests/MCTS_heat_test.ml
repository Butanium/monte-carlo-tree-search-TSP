(* run command :
   dune exec ./tests/MCTS_main.exe
*)
let city_config = "TSP100/instance1"

let city_count, cities = Reader_tsp.open_tsp city_config

let adj_matrix = Base_tsp.get_adj_matrix cities

let max_time = 10.

let max_simulation = max_int

let expected_length_policy = MCTS_Heat.Average

let optimization_policy =
  MCTS_Heat.Two_opt { max_iter = max_int; max_time = 1. } |> Option.some

let develop_simulation_policy = MCTS_Heat.Dev_all

let generate_log_file = 1

let optimize_end_path = false


let () =
  let _, (mcts_tour, _), _ =
    MCTS_Heat.proceed_mcts ~city_config ~expected_length_policy ~develop_simulation_policy
      ~optimization_policy ~generate_log_file ~optimize_end_path city_count
      adj_matrix max_time max_simulation
  in
  let tour_2opt = Iterated_2Opt.iter_two_opt ~adj_matrix ~city_count 10. max_int in 
  (* Graphics.sound 100 1000;
  Graphics.sound 200 1000; *)
  (*make some sound when monte carlo is complete *)
  print_endline "mcts error";
  Base_tsp.print_error_ratio mcts_tour adj_matrix city_config;
  print_endline "2opt error";
  Base_tsp.print_error_ratio tour_2opt adj_matrix city_config;
  print_endline "mcts tour : ";
  Base_tsp.print_tour mcts_tour;
  (* MCTS_Heat.show_heat_map hm |> Printf.printf "heat map : %s"; *)
  (* Show the computed solution *)
  print_endline "2opt tour : ";
  Base_tsp.print_tour tour_2opt;
  Show_tsp.show_tour_and_wait cities mcts_tour

let () = Show_tsp.show_best_tour_and_wait city_config
