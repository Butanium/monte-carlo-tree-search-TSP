(*run command :
  dune exec ./tests/two_opt_main.exe
*)

let city_config = "TSP100/instance1"
(* let city_config = "TSPLIB/att48" *)

let city_count, cities = Reader_tsp.open_tsp city_config

let adj_matrix = Base_tsp.get_adj_matrix cities

let tour =
  Random.self_init ();
  Base_tsp.random_tour city_count

let max_time = 10.

(* let () = Show_tsp.show_tour *)
let max_try = max_int
(* let tour = Iterated_2Opt.iter_two_opt adj_matrix city_count Iterated_2Opt.Roulette max_time max_try *)

(* let show () =
     let len = Base_tsp.tour_length adj_matrix tour in
     let best_len = Base_tsp.best_path_length city_config adj_matrix in
     Printf.printf "\n%% of error : %.2f %%\n"
       (100. *. float_of_int (len - best_len) /. float_of_int best_len);
     Base_tsp.print_tour tour;
     Show_tsp.show_tour_and_wait cities tour

   let () =
     show ();
     Two_Opt.opt_fast ~partial_path:true ~lower_bound:5 ~upper_bound:35 adj_matrix
       tour;
     show () *)

(* let crash_test =
  Iterated_2Opt.(
    iter_two_opt adj_matrix city_count ~seed:386772978 Random 5000. max_int
      ~logs_path:"logs/crash_test_2opt") *)
