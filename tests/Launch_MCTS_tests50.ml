(* open Simulations
   open All_tests_MCTS


   let file_path = "tsp_instances"

   let configs =
     let rec aux i =
       if i > 1 then []
       else (file_path, Printf.sprintf "TSP50/instance%d" i) :: aux (i + 1)
     in
     aux 1

   let opt_of_time max_time =
     MCTS.Two_opt { max_time; max_length = 50; max_iter = max_int }

   let base_opt = opt_of_time 1.

   let full_opt = MCTS.Full_Two_opt { max_time = 1.; max_iter = max_int }

   let models =
     All_tests_MCTS.create_models 5.
       ~opt_list:
         ([ MCTS.Random; Roulette ]
         $$ base_opt
            *$ [
                 ((1, 1), full_opt);
                 ((1, 2), No_opt);
                 ((1, 4), No_opt);
                 ((1, 2), opt_of_time 0.5);
                 ((1, 1), No_opt);
                 ((1, 2), full_opt);
               ]
            @ [ (full_opt, (1, 1), No_opt) ])
       ~vanilla_list:
         ([ MCTS.Roulette; Random ]
         $$ [ base_opt; No_opt; divide_opt 1 2 base_opt ])

   let () = All_tests_MCTS.run_models ~sim_name:"TSP50-experiment" configs models *)
