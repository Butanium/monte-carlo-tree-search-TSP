open Simulations
open Experiment_Runner

let file_path = "tsp_instances"

let test_set = 200

let amount = 128

let max_time = 1.

let sim_name = Printf.sprintf "Full-TSP%d-experiment" test_set

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

let dev_modes = MCTS.[ No_dev; Dev_all 5; Dev_hidden 5; Dev_playout 5 ]

let mcts_opt_list =
  MCTS.(
    dev_modes
    $$- ([ Random; Roulette ]
        $$ base_opt
           *$- (((1, 1), No_opt)
               :: ((1, 1), full_opt)
               :: ([ (1, 2); (1, 4) ] $$ [ No_opt; base_opt; full_opt ]))
           @ [ (full_opt, (1, 1), No_opt) ]))
  |> List.filter (fun (d, _, (_, _, h)) -> is_valid_dev h d)

let mcts_vanilla_list =
  MCTS.(
    dev_modes
    $$- ([ Roulette; Random ]
        $$ [ full_opt; base_opt; No_opt; divide_opt 1 2 base_opt ]))
  |> List.filter (fun (d, _, h) -> is_valid_dev h d)

let models =
  create_models max_time
    ~iter2opt_list:(max_int *$ [ Iterated_2Opt.Random; Roulette ])
    ~mcts_opt_list ~mcts_vanilla_list

let () = run_models ~sim_name configs models
