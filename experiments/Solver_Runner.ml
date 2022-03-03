type mcts_solver = {
  mutable name : string;
  max_time : float;
  exploration_mode : MCTS.exploration_mode;
  optimization_mode : MCTS.optimization_mode;
  selection_mode : MCTS.playout_selection_mode;
  hidden_opt : MCTS.optimization_mode;
  dev_mode : MCTS.develop_playout_mode;
}

type iterated2opt_solver = {
  name : string;
  max_time : float;
  max_iter : int;
  random_mode : Iterated_2Opt.random_creation;
}

type solver = MCTS of mcts_solver | Iter of iterated2opt_solver

let solver_name = function MCTS { name; _ } | Iter { name; _ } -> name

let solver_simulation ?(generate_log_file = 1) ?(verbose = 0) ?seed city_config
    city_count eval log_path =
  let log_files_path = Printf.sprintf "%s/%s" log_path city_config in
  function
  | MCTS solver ->
      let max_time = solver.max_time in
      let max_playout = 100000000 in
      let playout_selection_mode = solver.selection_mode in
      let exploration_mode = solver.exploration_mode in
      let expected_length_mode = MCTS.Average in
      let optimization_mode = solver.optimization_mode in
      let hidden_opt = solver.hidden_opt in
      let (_, length), (_, opt_length), _ =
        MCTS.proceed_mcts ~debug_tree:false ~city_config ~expected_length_mode
          ~playout_selection_mode ~exploration_mode ~optimization_mode
          ~generate_log_file ~stop_on_leaf:false ~optimize_end_path:true
          ~name:solver.name ~log_files_path ~verbose:(verbose - 1) ~hidden_opt
          ?seed ~catch_SIGINT:false city_count eval max_time max_playout
          ~develop_playout_mode:solver.dev_mode
      in
      (length, opt_length)
  | Iter solver ->
      let tour =
        Iterated_2Opt.iter_two_opt eval city_count solver.random_mode
          solver.max_time solver.max_iter ~name:solver.name ~city_config
          ~logs_path:log_files_path ~verbose:(verbose > 0) ?seed
      in
      let score = Base_tsp.tour_length eval tour in
      (score, score)
