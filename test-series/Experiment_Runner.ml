open Solver_Runner
open Util

let to_triple (a, (b, c)) = (a, b, c)

let ( $$ ) = Base.List.cartesian_product

let ( *$ ) a b = Base.List.cartesian_product [ a ] b

let ( *$- ) a b = List.map to_triple (a *$ b)

type model_experiment = {
  solver : solver;
  mutable experiment_count : int;
  mutable total_deviation : float;
  mutable total_length : int;
  mutable total_opted_length : int;
  mutable total_opted_deviation : float;
}

type named_opt = { opt : MCTS.optimization_mode; name : string }

let init_model solver =
  {
    solver;
    experiment_count = 0;
    total_deviation = 0.;
    total_length = 0;
    total_opted_length = 0;
    total_opted_deviation = 0.;
  }

let string_of_ratio = function
  | 2 -> "Semi"
  | 4 -> "Quart"
  | n -> Printf.sprintf "1/%d" n

let prefixOpt total_factor length_factor =
  match (total_factor, length_factor) with
  | 1, 1 -> "Base"
  | 1, x -> string_of_ratio x ^ "Length"
  | x, 1 -> string_of_ratio x ^ "Duration"
  | l, d -> string_of_ratio l ^ "Length_" ^ string_of_ratio d ^ "Duration"

let divide_opt total_factor length_factor = function
  | MCTS.Two_opt opt ->
      MCTS.Two_opt
        {
          max_length = opt.max_length / length_factor;
          max_time = opt.max_time /. float total_factor;
          max_iter = opt.max_iter / total_factor;
        }
  | MCTS.Full_Two_opt opt ->
      MCTS.Full_Two_opt
        {
          max_time = opt.max_time /. float total_factor;
          max_iter = opt.max_iter / total_factor;
        }
  | x -> x

let name_opt total_factor length_factor = function
  | MCTS.Two_opt _ ->
      Printf.sprintf "%s2Opt" @@ prefixOpt total_factor length_factor
  | MCTS.Full_Two_opt _ ->
      Printf.sprintf "%sFull2Opt" @@ prefixOpt total_factor length_factor
  | No_opt -> "NoOpt"
  | _ -> assert false

let create_mcts_opt total_factor length_factor opt =
  let opt = divide_opt total_factor length_factor opt in
  let name = name_opt total_factor length_factor opt in
  { opt; name }

let opt_of_tuple (opt, (total_factor, length_factor)) =
  create_mcts_opt total_factor length_factor opt

let def_opt = create_mcts_opt 1 1

let create_models ?(exploration_mode = MCTS.Standard_deviation)
    ?(mcts_vanilla_list = []) ?(mcts_opt_list = []) ?(iter2opt_list = [])
    max_time =
  let create_opt_mcts (selection_mode, (opt, t, hidden_opt)) =
    let { opt; name } = opt_of_tuple (opt, t) in
    MCTS
      {
        name =
          Printf.sprintf "MCTS-%s-%s%s" name
            (MCTS.str_of_selection_mode selection_mode)
            (if hidden_opt = MCTS.No_opt then ""
            else
              Printf.sprintf "-hidden_%s"
              @@ MCTS.str_of_optimization_mode_short hidden_opt);
        max_time;
        exploration_mode;
        optimization_mode = opt;
        selection_mode;
        hidden_opt;
      }
  in
  let create_vanilla_mcts (selection_mode, hidden_opt) =
    MCTS
      {
        name =
          Printf.sprintf "MCTS-Vanilla-%s%s"
            (MCTS.str_of_selection_mode selection_mode)
            (if hidden_opt = MCTS.No_opt then ""
            else
              Printf.sprintf "-hidden_%s"
              @@ MCTS.str_of_optimization_mode_short hidden_opt);
        max_time;
        exploration_mode;
        optimization_mode = No_opt;
        selection_mode = Random;
        hidden_opt;
      }
  in
  let create_iterated_opt (max_iter, random_mode) =
    Iter
      {
        max_iter;
        max_time;
        random_mode;
        name =
          Printf.sprintf "Iterated2Opt-%s%s"
            (Two_Opt.string_of_random_mode random_mode)
            (if max_iter = max_iter then ""
            else Printf.sprintf "-%diters" max_iter);
      }
  in
  List.map init_model
    (List.map create_opt_mcts mcts_opt_list
    @ List.map create_vanilla_mcts mcts_vanilla_list
    @ List.map create_iterated_opt iter2opt_list)

let run_models ?(sim_name = "sim") ?(mk_new_log_dir = true) ?(verbose = -1)
    configs models =
  let start_time = Sys.time () in
  let last_debug = ref start_time in
  let debug_count = ref 0 in
  Printf.printf "\nRunning sim %s...\n%!" sim_name;
  let path = Printf.sprintf "logs/%s" sim_name in
  let log_files_path =
    if mk_new_log_dir then File_log.create_log_dir path else path
  in
  List.iter
    (fun (file_path, config) ->
      let city_count, cities = Reader_tsp.open_tsp ~file_path config in
      let eval = Base_tsp.dists cities in
      let objective_length =
        float @@ Base_tsp.best_path_length ~file_path config eval
      in
      List.iter
        (fun model ->
          if verbose > 0 && Sys.time () -. !last_debug > 60. then (
            incr debug_count;
            Printf.printf
              "currently testing %s, has been running for %d minutes\n%!" config
              !debug_count;
            last_debug := Sys.time ());
          let length, opt_length =
            solver_simulation config city_count eval log_files_path model.solver
              ~verbose:(verbose - 1)
          in
          model.experiment_count <- model.experiment_count + 1;
          model.total_deviation <-
            model.total_deviation
            +. ((float length -. objective_length) /. objective_length);
          model.total_length <- model.total_length + length;
          model.total_opted_deviation <-
            model.total_opted_deviation
            +. ((float opt_length -. objective_length) /. objective_length);
          model.total_opted_length <- model.total_opted_length + opt_length)
        models)
    configs;
  let file_name = "all_mcts_tests-" ^ sim_name in
  let logs = File_log.create_file ~file_path:log_files_path ~file_name () in
  let oc =
    File_log.log_single_data ~close:false logs
      "solver-name,average-deviation,average-length,average-opted-deviation,average-opted-length"
  in
  File_log.log_data_oc
    (fun model ->
      let n = float model.experiment_count in
      let mean_s x = strg (x /. n) in
      Printf.sprintf "%s,%s,%s,%s,%s\n" (solver_name model.solver)
        (mean_s model.total_deviation)
        (mean_s @@ float model.total_length)
        (mean_s model.total_opted_deviation)
        (mean_s @@ float model.total_opted_length))
    oc
  @@ List.sort (fun a b -> compare a.total_deviation b.total_deviation) models;
  Printf.printf
    "\n\nExperiment ended in %g seconds\nResult file available at : %s%s"
    (Sys.time () -. start_time)
    logs.file_path logs.file_name
