(* open Simulations

let file_path = "tsp_instances"

let configs =
  let rec aux i =
    if i > 4 then []
    else (file_path, Printf.sprintf "TSP20/instance%d" i) :: aux (i + 1)
  in
  aux 1

let base_opt = MCTS.Two_opt { max_time = 1.; max_length = 20; max_iter = max_int }

let models =
  All_tests_MCTS.create_models 2. ~base_opt
    ~vanilla_list:(Base.List.cartesian_product [MCTS.Roulette; Random] [base_opt; No_opt])
    ~opt_list:[Roulette, ((1,1), No_opt); Random, ((1,1), No_opt)]

let () = All_tests_MCTS.run_models ~sim_name:"TSP20-experiment4" configs models *)
