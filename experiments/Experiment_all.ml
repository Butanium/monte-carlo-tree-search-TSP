let to_triple (a, (b, c)) = (a, b, c)

let ( $$ ) = Base.List.cartesian_product

let ( *$ ) a b = Base.List.cartesian_product [ a ] b

let ( *$- ) a b = List.map to_triple (a *$ b)

let ( $$- ) a b = List.map to_triple (a $$ b)

let is_valid_dev hidden_policy dev_policy =
  let open MCTS in
  match (hidden_policy, dev_policy) with
  | No_opt, Dev_all _ -> false
  | No_opt, Dev_hidden _ -> false
  | _ -> true

let file_path = "tsp_instances"

let default_time = 1.

let base_opt =
  MCTS.Two_opt { max_time = 1.; max_length = 100; max_iter = max_int }

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
        $$ [
             full_opt;
             base_opt;
             No_opt;
             Experiment_Runner.divide_opt 1 2 base_opt;
           ]))
  |> List.filter (fun (d, _, h) -> is_valid_dev h d)

let greedy_list = Greedy_Random.[ (max_int, Roulette); (max_int, Random) ]

let models max_time exploration_policy =
  Experiment_Runner.create_models max_time
    ~iter2opt_list:(max_int *$ [ Iterated_2Opt.Random; Roulette ])
    ~mcts_opt_list ~mcts_vanilla_list ~greedy_list ~exploration_policy

let experiment_all ?sim_name ?(amount = 128) ?(test_set = 200)
    ?(max_time = default_time) ?(exp_per_config = 1)
    ?(exploration_policy = MCTS.Standard_deviation 1.) () =
  let configs =
    let rec aux i =
      if i > amount then []
      else
        (file_path, Printf.sprintf "TSP%d/instance%d" test_set i) :: aux (i + 1)
    in
    aux 1
  in
  let sim_name =
    match sim_name with
    | None -> Printf.sprintf "Duration-%.3gs-TSP%d" max_time test_set
    | Some s -> s
  in
  let models = models max_time exploration_policy in
  Experiment_Runner.run_models ~sim_name configs models ~exp_per_config
