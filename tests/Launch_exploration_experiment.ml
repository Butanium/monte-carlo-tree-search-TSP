let max_time = 30.

let exploration_policies =
  MCTS.
    [
      Standard_deviation 0.01;
      Standard_deviation 0.1;
      Standard_deviation 0.5;
      Standard_deviation 1.;
      Min_spanning_tree 0.01;
      Min_spanning_tree 0.1;
      Min_spanning_tree 0.5;
      Min_spanning_tree 1.;
      Standard_deviation 2.;
      Min_spanning_tree 2.;
    ]

(* 
  # First exploration experiment

  let () =
  List.iter
    (fun exploration_policy ->
      Experiments.Experiment_Generator.experiment_partial ~exploration_policy ~max_time
        ~amount:128 ~test_set:100 ~exp_per_config:5 ())
    exploration_policies 
*)

(* # Second exploration experiment *)
let () =
  List.iter
    (fun exploration_policy ->
      Experiments.Experiment_Generator.experiment_partial ~exploration_policy ~max_time
        ~amount:128 ~test_set:100 ~exp_per_config:5 ~ignore_level:3 ())
    exploration_policies
