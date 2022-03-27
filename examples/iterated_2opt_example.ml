(*run command : dune exec ./examples/iterated_2opt_example.exe *)

let city_config = "a280"

let city_count, cities = Reader_tsp.open_tsp city_config

let adj_matrix = Base_tsp.get_adj_matrix cities

let max_time = 30.

let max_try = 100

let two_path, (mcts_path, _, _) =
  let t = Unix.gettimeofday () in
  Iterated_2Opt.
    ( iter_two_opt adj_matrix city_count Random max_time max_try,
      MCTS.proceed_mcts
        ~optimization_policy:
          (MCTS.Two_opt { max_length = 400; max_iter = 400; max_time = 1. })
        city_count adj_matrix
        (Unix.gettimeofday () -. t)
        max_int )

let () =
  Show_tsp.show_solution_and_wait ~title:"two-opt tour" cities two_path;
  Base_tsp.print_error_ratio two_path adj_matrix city_config;
  Show_tsp.show_solution_and_wait ~title:"two-opt tour" cities two_path
