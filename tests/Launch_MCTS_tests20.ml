open Simulations
open All_tests_MCTS

let file_path = "tsp_instances"

let experiment_id = 1

let test_set = 20

let amount = 128

let sim_name = Printf.sprintf "TSP%d-experiment%d" test_set experiment_id

let configs =
  let rec aux i =
    if i > amount then []
    else (file_path, Printf.sprintf "TSP%d/instance%d" test_set i) :: aux (i + 1)
  in
  aux 1

let opt_of_time max_time =
  MCTS.Two_opt { max_time; max_length = 20; max_iter = max_int }

let base_opt = opt_of_time 1.

let full_opt = MCTS.Full_Two_opt { max_time = 1.; max_iter = max_int }

let models =
  All_tests_MCTS.create_models 2.
    ~opt_list:
      ([ MCTS.Random; Roulette ]
      $$ base_opt
         *$ [
              ((1, 1), full_opt);
              ((1, 2), No_opt);
              ((1, 2), opt_of_time 0.5);
              ((1, 1), No_opt);
              ((1, 2), full_opt);
            ]
         @ [ (full_opt, (1, 1), No_opt) ])
    ~vanilla_list:([ MCTS.Roulette; Random ] $$ [ full_opt; base_opt; No_opt; divide_opt 1 2 base_opt ])

let () = All_tests_MCTS.run_models ~sim_name configs models
