open Simulations
open Run_tests

let file_path = "tsp_instances"
let max_time = 10.
let test_set = 50

let amount = 2

let sim_name = Printf.sprintf "TSP%d-experimentTest" test_set
let configs =
  let rec aux i =
    if i > amount then []
    else
      (file_path, Printf.sprintf "TSP%d/instance%d" test_set i) :: aux (i + 1)
  in
  aux 1

let base_opt =
  MCTS.Two_opt { max_time = 1.; max_length = 50; max_iter = max_int }

let full_opt = MCTS.Full_Two_opt { max_time = 1.; max_iter = max_int }

(* let models =
   All_tests_MCTS.create_models 2.
     ~vanilla_list:(Base.List.cartesian_product [MCTS.Roulette; Random] [MCTS.No_opt]) *)
(* let models =
  create_models 1.
    ~mcts_opt_list:([ MCTS.Roulette; Random ] $$ [ (base_opt, (1, 1), full_opt);
    (full_opt, (1,1), No_opt) ]) *)
let models = create_models max_time ~iter2opt_list: (max_int *$ [Two_Opt.Random; Roulette]) 

let () = run_models ~sim_name configs models
