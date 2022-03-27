let durations = [ 0.1; 0.25; 0.5; 1.; 2. ]

let linux_duration = List.map (fun x -> x *. 60.) durations

let () =
  let durations = if Sys.os_type == "Unix" then linux_duration else durations in
  List.iter
    (fun max_time ->
      Experiments.Experiment.experiment_all ~max_time ~amount:50 ~test_set:50 ())
    durations
