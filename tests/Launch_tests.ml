open Simulations
open Experiment_Runner

let file_path = "tsp_instances"

let test_set = 100

let amount = 1

let max_time = 2.

let sim_name = Printf.sprintf "TSP%d-experiment" test_set

let configs =
  let rec aux i =
    if i > amount then []
    else (file_path, Printf.sprintf "TSP%d/instance%d" test_set i) :: aux (i + 1)
  in
  aux 1

let opt_of_time max_time =
  MCTS.Two_opt { max_time; max_length = 100; max_iter = max_int }

let base_opt = opt_of_time 1.

let full_opt = MCTS.Full_Two_opt { max_time = 1.; max_iter = max_int }

let models =
  create_models max_time
    ~iter2opt_list:(max_int *$ [ Two_Opt.Random; Roulette ])
    ~mcts_opt_list:
      ([ MCTS.Random; Roulette ]
      $$ base_opt
         *$- (((1, 1), MCTS.No_opt) :: ((1,1), full_opt)
             :: ([ (1, 2); (1, 4) ] $$ [ MCTS.No_opt; base_opt; full_opt ]))
         @ [ (full_opt, (1, 1), No_opt) ])
    ~mcts_vanilla_list:
      ([ MCTS.Roulette; Random ]
      $$ [ full_opt; base_opt; No_opt; divide_opt 1 2 base_opt ])

let () = run_models ~sim_name configs models 
