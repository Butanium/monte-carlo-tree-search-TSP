type mcts_solver = {
  mutable name : string;
  max_time : float;
  exploration_mode : MCTS.exploration_mode;
  optimization_mode : MCTS.optimization_mode;
  selection_mode : MCTS.playout_selection_mode;
  hidden_opt : MCTS.optimization_mode;
}

type iterated2opt_solver = {
  name : string;
  max_time : float;
  max_iter : int;
  random_mode : Two_Opt.random_creation;
}

type solver = MCTS of mcts_solver | Iter of iterated2opt_solver

let solver_name = function MCTS { name; _ } | Iter { name; _ } -> name

let solver_simulation city_config city_count eval log_path =
  let log_files_path = Printf.sprintf "%s/%s" log_path city_config in
  function
  | MCTS solver ->
      let max_time = solver.max_time in
      let max_playout = 100000000 in
      let playout_selection_mode = solver.selection_mode in
      let exploration_mode = solver.exploration_mode in
      let expected_length_mode = MCTS.Average in
      let optimization_mode = solver.optimization_mode in
      let generate_log_file = true in
      let debug_tree = false in
      let optimize_end_path = true in
      let stop_on_leaf = false in
      let hidden_opt = solver.hidden_opt in
      let (_, length), (_, opt_length), _ =
        MCTS.proceed_mcts ~debug_tree ~city_config ~expected_length_mode
          ~playout_selection_mode ~exploration_mode ~optimization_mode
          ~generate_log_file ~stop_on_leaf ~optimize_end_path ~name:solver.name
          ~log_files_path ~verbose:(-1) ~hidden_opt ~catch_SIGINT:false
          city_count eval max_time
          max_playout
      in
      (length, opt_length)
  | Iter solver ->
      let path =
        Two_Opt.iter_two_opt eval city_count solver.random_mode solver.max_time
          solver.max_iter ~name:solver.name ~city_config ~logs_path:log_files_path ~verbose:false
      in
      let score = Base_tsp.path_length eval path in
      (score, score)
