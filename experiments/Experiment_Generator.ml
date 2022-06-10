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

let dev_modes =
  MCTS.
    [
      Dev_hidden 5; Dev_simulation 5; No_dev; Dev_all 5; Dev_hidden 0; Dev_all 0;
    ]

let dev_modes_small = MCTS.[ Dev_hidden 5; Dev_all 5; Dev_hidden 0; Dev_all 0 ]

let all_iter2opt_list =
  max_int
  *$- Iterated_2Opt.([ Random; Roulette ] $$ Two_Opt.[ Fast; Best; First ])

let single_opt_list =
  1 *$- Iterated_2Opt.([ Random; Roulette ] $$ Two_Opt.[ Fast; Best; First ])

let configs test_set amount =
  let rec aux i =
    if i > amount then []
    else (file_path, Printf.sprintf "TSP%d/instance%d" test_set i) :: aux (i + 1)
  in
  aux 1

let experiment_iter2opt ?sim_name ?(amount = 128) ?(test_set = 200)
    ?(max_time = default_time) ?(exp_per_config = 1) ?(exp_type = `Both) () =
  let create = Experiment_Runner.create_models ~max_time in
  let models =
    match exp_type with
    | `Iterated -> create ~iter2opt_list:all_iter2opt_list ()
    | `Single -> create ~iter2opt_list:single_opt_list ()
    | `Both -> create ~iter2opt_list:(all_iter2opt_list @ single_opt_list) ()
  in

  let sim_name =
    match sim_name with
    | None -> Printf.sprintf "Iter2Opt-TSP%d-%.3gs" test_set max_time
    | Some s -> s
  in

  let configs = configs test_set amount in
  Experiment_Runner.run_models ~sim_name configs models ~exp_per_config

let experiment_partial ?sim_name ?(amount = 128) ?(test_set = 200)
    ?(max_time = default_time) ?(exp_per_config = 1)
    ?(exploration_policy = MCTS.Standard_deviation 1.) ?(ignore_level = 0)
    ?(score_policy = MCTS.Average) () =
  let base_opt =
    MCTS.Two_opt { max_time = 1.; max_length = test_set; max_iter = max_int }
  in

  let full_opt = MCTS.Full_Two_opt { max_time = 1.; max_iter = max_int } in

  let mcts_opt_list =
    (match ignore_level with
    | 0 ->
        MCTS.(
          dev_modes
          $$- ([ Random; Roulette ]
              $$ (base_opt *$- [ ((1, 1), No_opt); ((1, 1), full_opt) ])
                 @ [ (full_opt, (1, 1), No_opt) ]))
    | 1 ->
        MCTS.(
          dev_modes
          $$- ([ Random; Roulette ] $$ [ (base_opt, (1, 1), full_opt) ]))
    | _ ->
        MCTS.(
          dev_modes_small
          $$- ([ Random; Roulette ] $$ [ (base_opt, (1, 1), full_opt) ])))
    |> List.filter (fun (d, _, (_, _, h)) -> is_valid_dev h d)
  in

  let mcts_vanilla_list =
    match ignore_level with
    | 0 ->
        MCTS.(
          dev_modes $$- ([ Roulette; Random ] $$ [ full_opt; base_opt; No_opt ]))
        |> List.filter (fun (d, _, h) -> is_valid_dev h d)
    | 1 ->
        MCTS.(dev_modes $$- ([ Roulette; Random ] $$ [ full_opt ]))
        |> List.filter (fun (d, _, h) -> is_valid_dev h d)
    | _ -> []
  in

  let models =
    Experiment_Runner.create_models ~max_time ~mcts_opt_list ~mcts_vanilla_list
      ~exploration_policy ~score_policy ()
  in

  let sim_name =
    match sim_name with
    | None ->
        Printf.sprintf "Exploration-%s-TSP%d-%.3gs"
          (MCTS.str_of_exploration_policy exploration_policy)
          test_set max_time
    | Some s -> s
  in

  let configs = configs test_set amount in

  Experiment_Runner.run_models ~sim_name configs models ~exp_per_config

let experiment_all ?sim_name ?(amount = 128) ?(test_set = 200)
    ?(exp_per_config = 1) ?(exploration_policy = MCTS.Standard_deviation 1.)
    ?(max_time = default_time) () =
  let greedy_list = Greedy_Random.[ (max_int, Roulette); (max_int, Random) ] in

  let base_opt =
    MCTS.Two_opt { max_time = 1.; max_length = test_set; max_iter = max_int }
  in

  let full_opt = MCTS.Full_Two_opt { max_time = 1.; max_iter = max_int } in

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
  in

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
  in

  let models =
    Experiment_Runner.create_models ~max_time ~iter2opt_list:all_iter2opt_list
      ~mcts_opt_list ~mcts_vanilla_list ~greedy_list ~exploration_policy ()
  in

  let configs = configs test_set amount in

  let sim_name =
    match sim_name with
    | None -> Printf.sprintf "Duration-%.3gs-TSP%d" max_time test_set
    | Some s -> s
  in

  Experiment_Runner.run_models ~sim_name configs models ~exp_per_config
