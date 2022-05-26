(*
   Run a solver on a given instance and return the score of the best tour.
*)

type mcts_solver = {
  mutable name : string;
  max_time : float;
  exploration_policy : MCTS.exploration_policy;
  optimization_policy : MCTS.optimization_policy;
  selection_policy : MCTS.simulation_selection_policy;
  hidden_opt : MCTS.optimization_policy;
  dev_policy : MCTS.develop_simulation_policy;
  score_policy : MCTS.expected_length_policy;
}

type iterated2opt_solver = {
  name : string;
  max_time : float;
  max_iter : int;
  random_policy : Iterated_2Opt.random_policy;
}

type greedy_solver = {
  name : string;
  max_time : float;
  max_iter : int;
  random_policy : Greedy_Random.random_policy;
}

type solver =
  | MCTS of mcts_solver
  | Iter of iterated2opt_solver
  | Greedy of greedy_solver
  | Exact

let solver_name = function
  | MCTS { name; _ } | Iter { name; _ } | Greedy { name; _ } -> name
  | Exact -> "Exact-Solver"

let solver_simulation ?(generate_log_file = 1) ?(verbose = 0) ?seed city_config
    city_count adj_matrix log_path =
  let log_files_path = Printf.sprintf "%s/%s" log_path city_config in
  function
  | Exact -> (-1, -1) (* Experiement Runner handles it *)
  | MCTS solver ->
      let max_time = solver.max_time in
      let max_simulation = 100000000 in
      let simulation_selection_policy = solver.selection_policy in
      let exploration_policy = solver.exploration_policy in
      let expected_length_policy = solver.score_policy in
      let optimization_policy = solver.optimization_policy in
      let hidden_opt = solver.hidden_opt in
      let (_, length), (_, opt_length), _ =
        MCTS.proceed_mcts ~debug_tree:false ~city_config ~expected_length_policy
          ~simulation_selection_policy ~exploration_policy ~optimization_policy
          ~generate_log_file ~stop_on_leaf:false ~optimize_end_path:true
          ~name:solver.name ~log_files_path ~verbose:(verbose - 1) ~hidden_opt
          ?seed ~catch_SIGINT:false ~city_count ~adj_matrix max_time max_simulation
          ~develop_simulation_policy:solver.dev_policy
      in
      (length, opt_length)
  | Iter solver ->
      let tour =
        Iterated_2Opt.iter_two_opt adj_matrix city_count solver.random_policy
          solver.max_time solver.max_iter ~name:solver.name ~city_config
          ?logs_path:
            (if generate_log_file < 0 then None else Some log_files_path)
          ~verbose:(verbose > 0) ?seed
      in
      let score = Base_tsp.tour_length adj_matrix tour in
      (score, score)
  | Greedy solver ->
      let score, _ =
        Greedy_Random.greedy adj_matrix city_count solver.random_policy
          solver.max_time solver.max_iter ~logs_path:log_files_path
          ~generate_log_file ~verbose:(verbose - 1)
      in
      (score, score)
