(*run command :
  dune exec ./tests/two_opt_main.exe
*)

let city_config = "att48"

let city_count, cities = Reader_tsp.open_tsp city_config

let eval = Base_tsp.dists cities

let path =
  Random.self_init ();
  Base_tsp.random_path city_count

let max_time = 10.

let max_try = max_int
(* let path = Two_Opt.iter_two_opt eval city_count Two_Opt.Roulette max_time max_try *)

let show () =
  let len = Base_tsp.path_length eval path in
  let best_len = Base_tsp.best_path_length city_config eval in
  Printf.printf "\n%% of error : %.2f %%\n"
    (100. *. float_of_int (len - best_len) /. float_of_int best_len);
  Base_tsp.print_path path;
  Show_tsp.show_solution_and_wait cities path

let () =
  show ();
  Two_Opt.opt_fast ~partial_path:true ~lower_bound:5 ~upper_bound:35 eval path;
  show ()
