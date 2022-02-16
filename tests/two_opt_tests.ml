(*run command :
  dune exec ./tests/two_opt_main.exe
*)

let city_config = "TSP100/instance2"

let city_count, cities = Reader_tsp.open_tsp city_config

let adj_matrix = Base_tsp.get_adj_matrix cities

let path =
  Random.self_init ();
  Base_tsp.random_path city_count

let max_time = 10.

let max_try = max_int
(* let path = Two_Opt.iter_two_opt adj_matrix city_count Two_Opt.Roulette max_time max_try *)

(* let show () =
     let len = Base_tsp.path_length adj_matrix path in
     let best_len = Base_tsp.best_path_length city_config adj_matrix in
     Printf.printf "\n%% of error : %.2f %%\n"
       (100. *. float_of_int (len - best_len) /. float_of_int best_len);
     Base_tsp.print_path path;
     Show_tsp.show_solution_and_wait cities path

   let () =
     show ();
     Two_Opt.opt_fast ~partial_path:true ~lower_bound:5 ~upper_bound:35 adj_matrix
       path;
     show () *)

let crash_test =
  Two_Opt.(
    iter_two_opt adj_matrix city_count ~seed:386772978 Random 5000. max_int
      ~logs_path:"logs/crash_test_2opt")
