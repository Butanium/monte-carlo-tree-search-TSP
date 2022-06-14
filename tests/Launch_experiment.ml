(*
   - For all experiments :
    let () = Experiments.Experiment_Generator.experiment_all ()
*)

(*
    - For iterated 2-Opt experiments :
*)
(* let () =
   Experiments.Experiment_Generator.experiment_iter2opt ~max_time:10. ~amount:128
     ~test_set:100 ~exp_per_config:5 ~exp_type:`Iterated () *)

(*
    - For Greedy experiments :   
*)

let () = Experiments.Experiment_Generator.greedy_experiment ~test_set:100 ~max_time:30. ()
