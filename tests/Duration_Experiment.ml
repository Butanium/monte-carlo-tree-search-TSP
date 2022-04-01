(* let durations = [ 0.01; 0.05; 0.1; 0.25; 0.5; 1.; 2. ] *)
let durations = [ 0.01; 0.05 ; 0.1 ; 0.5 ; 1. ; 2. ; 10. ; 60. ]
(* let linux_duration = List.map (fun x -> x *. 60.) durations *)

let () =
  (* let durations = if Sys.os_type == "Unix" then linux_duration else durations in *)
  List.iter
    (fun max_time ->
      Experiments.Experiment_all.experiment_all ~max_time ~amount:128 ~test_set:100 ~exp_per_config:5 ())
    durations
