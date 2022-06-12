(*
   Run several experiments with different solver (iterated 2 Opt, MCTS, Greedy)
   This is what I use to compare the results of the different algorithms.
*)

open Solver_Runner

type model_experiment = {
  solver : solver;
  mutable experiment_count : int;
  mutable lengths : int list;
  mutable opted_lengths : int list;
}

type model_result = {
  model : model_experiment;
  deviation : float;
  length : float;
  opt_deviation : float;
  opt_length : float;
  deviation_standart_dev : float;
  opt_deviation_standart_dev : float;
  min_dev : float;
  max_dev : float;
  opt_min_dev : float;
  opt_max_dev : float;
}

let get_model_results ?(missing_exp = 0) best_lengths model exp_per_config =
  let n = float model.experiment_count in
  let mean_list list = List.fold_left ( +. ) 0. list /. n in
  let get_deviations lengths =
    let rec aux exp_i best_lengths lengths acc =
      match (best_lengths, lengths) with
      | bl :: bls, l :: ls ->
          if exp_i = exp_per_config then aux 0 bls lengths acc
          else
            let diff = float (l - bl) /. float bl in
            assert (l - bl >= 0);
            aux (exp_i + 1) best_lengths ls (diff :: acc)
      | _ -> acc
    in
    aux missing_exp best_lengths lengths []
  in
  let deviations = get_deviations model.lengths in
  let opt_deviations = get_deviations model.opted_lengths in
  let deviation = mean_list deviations in
  let length = model.lengths |> List.map float |> mean_list in
  let opt_deviation = mean_list opt_deviations in
  let opt_length = model.opted_lengths |> List.map float |> mean_list in
  let dev_of_dev l =
    List.fold_left (fun acc dev -> acc +. ((deviation -. dev) ** 2.)) 0. l /. n
    |> sqrt
  in
  let deviation_standart_dev = dev_of_dev deviations in
  let opt_deviation_standart_dev = dev_of_dev opt_deviations in
  let min_dev = Base.List.min_elt deviations ~compare |> Option.get in
  let max_dev = Base.List.max_elt deviations ~compare |> Option.get in
  let opt_min_dev = Base.List.min_elt opt_deviations ~compare |> Option.get in
  let opt_max_dev = Base.List.max_elt opt_deviations ~compare |> Option.get in
  {
    model;
    deviation;
    length;
    opt_deviation;
    opt_length;
    deviation_standart_dev;
    opt_deviation_standart_dev;
    min_dev;
    max_dev;
    opt_max_dev;
    opt_min_dev;
  }

type named_opt = { opt : MCTS.optimization_policy; name : string }

let init_model solver =
  { solver; experiment_count = 0; lengths = []; opted_lengths = [] }

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

(** Create model record which will be run by the Solver_Runner module *)
let create_models ?(exploration_policy = MCTS.Standard_deviation 1.)
    ?(mcts_vanilla_list = []) ?(mcts_opt_list = []) ?(iter2opt_list = [])
    ?(greedy_list = []) ?(score_policy = MCTS.Average) ~max_time () =
  let suffix hidden_opt dev_policy =
    (if hidden_opt = MCTS.No_opt then ""
    else
      Printf.sprintf "-hidden_%s"
      @@ MCTS.str_of_optimization_policy_short hidden_opt)
    ^
    if dev_policy = MCTS.No_dev then ""
    else "-" ^ MCTS.str_of_develop_policy dev_policy
  in
  let create_opt_mcts (dev_policy, selection_policy, (opt, t, hidden_opt)) =
    let { opt; name } = opt_of_tuple (opt, t) in
    MCTS
      {
        name =
          Printf.sprintf "MCTS-%s-%s%s" name
            (MCTS.str_of_selection_policy selection_policy)
          @@ suffix hidden_opt dev_policy;
        max_time;
        exploration_policy;
        optimization_policy = opt;
        selection_policy;
        hidden_opt;
        dev_policy;
        score_policy;
      }
  in
  let create_vanilla_mcts (dev_policy, selection_policy, hidden_opt) =
    MCTS
      {
        name =
          Printf.sprintf "MCTS-Vanilla-%s%s"
            (MCTS.str_of_selection_policy selection_policy)
          @@ suffix hidden_opt dev_policy;
        max_time;
        exploration_policy;
        optimization_policy = No_opt;
        selection_policy = Random;
        hidden_opt;
        dev_policy;
        score_policy;
      }
  in
  let create_iterated_opt ?name (max_iter, random_policy, opt_policy) =
    Iter
      {
        max_iter;
        max_time;
        random_policy;
        opt_policy;
        name =
          Iterated_2Opt.(
            match name with
            | Some n -> n
            | None ->
                if max_iter <> 1 then
                  Printf.sprintf "Iterated2Opt-%s-%s%s"
                    (string_of_random_policy random_policy)
                    (Two_Opt.string_of_opt_policy opt_policy)
                    (if max_iter = max_int then ""
                    else Printf.sprintf "-%diters" max_iter)
                else
                  Printf.sprintf "1%s-%s"
                    (string_of_random_policy random_policy)
                    (Two_Opt.string_of_opt_policy opt_policy));
      }
  in
  let create_greedy ?name (max_iter, random_policy) =
    Greedy
      {
        max_iter;
        max_time;
        random_policy;
        name =
          (match name with
          | Some n -> n
          | None ->
              Printf.sprintf "Greedy-%s%s"
                (Greedy_Random.string_of_random_policy random_policy)
                (if max_iter = max_iter then ""
                else Printf.sprintf "-%diters" max_iter));
      }
  in
  List.map init_model
    ((Exact :: List.map create_opt_mcts mcts_opt_list)
    @ List.map create_vanilla_mcts mcts_vanilla_list
    @ List.map create_iterated_opt iter2opt_list
    @ List.map create_greedy greedy_list)

(** Run the different models and collect their results *)
let run_models ?(sim_name = "sim") ?(mk_new_log_dir = true) ?(verbose = 1) ?seed
    ?(exp_per_config = 1) configs models =
  let exception Break of int list in
  let update_csv = ref false in
  if Sys.os_type <> "Win32" then
    Sys.set_signal Sys.sigusr1
      (Sys.Signal_handle
         (fun _ ->
           Printf.printf
             "Update csv signal received ! Waiting for last model result...\n%!";
           update_csv := true));
  let stop_experiment = ref false in
  if Sys.os_type <> "Win32" then
    Sys.set_signal Sys.sigusr2
      (Sys.Signal_handle
         (fun _ ->
           Printf.printf
             "Stop experiment signal received ! Waiting for last model result...\n\
              %!";
           stop_experiment := true));
  let start_time = Unix.gettimeofday () in
  let last_debug = ref start_time in
  let debug_count = ref 0 in
  let file_path = Printf.sprintf "logs/%s" sim_name in
  let log_files_path =
    if mk_new_log_dir then File_log.create_log_dir file_path else file_path
  in
  let file_name = "all_mcts_tests-" ^ sim_name in
  let logs = File_log.create_file ~file_path:log_files_path ~file_name () in

  let update_log_file ?(missing_exp = 0) best_lengths =
    let first_row =
      "solver-name,average-deviation,standard-deviation-deviation,average-length,average-opted-deviation,standard-deviation-deviation,average-opted-length,max-deviation,min-deviation,opt-max-deviation,opt-min-deviation"
    in
    Printf.printf "%s\n%!" first_row;
    let oc = File_log.log_string_endline ~close:false ~file:logs first_row in
    ignore
      (List.filter_map
         (fun model ->
           if model.experiment_count = 0 then None
           else
             Some
               (get_model_results ~missing_exp best_lengths model exp_per_config))
         models
      |> List.sort (fun a b -> compare a.opt_deviation b.opt_deviation)
      |> File_log.log_data
           (fun result ->
             if result.model.experiment_count = 0 then ""
             else
               let row =
                 Printf.sprintf "%s,%g,%g,%g,%g,%g,%g,%g,%g,%g,%g\n"
                   (solver_name result.model.solver)
                   result.deviation result.deviation_standart_dev result.length
                   result.opt_deviation result.opt_deviation_standart_dev
                   result.opt_length result.max_dev result.min_dev
                   result.opt_max_dev result.opt_min_dev
               in

               Printf.printf "%s\n%!" row;
               row)
           ~oc)
  in

  Printf.printf "\nRunning sim %s...\n%!"
    (Scanf.sscanf log_files_path "logs/%s" Fun.id);
  let best_lengths =
    try
      configs
      |> List.fold_left
           (fun best_lengths (file_path, config) ->
             let city_count, cities = Reader_tsp.open_tsp ~file_path config in
             let adj = Base_tsp.get_adj_matrix cities in
             let best_tour_length =
               Base_tsp.best_tour_length ~file_path config adj
             in
             let best_lengths = best_tour_length :: best_lengths in
             for i = 1 to exp_per_config do
               models
               |> List.iter (fun model ->
                      (* ___ Get model results ___ *)
                      let length, opt_length =
                        if model.solver = Exact then
                          (best_tour_length, best_tour_length)
                        else
                          solver_simulation config city_count adj log_files_path
                            model.solver ~verbose:(verbose - 1)
                            ~generate_log_file:(if i = 1 then 3 else 1)
                            ?seed
                      in

                      (* ___ Update model with new results ___ *)
                      model.experiment_count <- model.experiment_count + 1;
                      model.lengths <- length :: model.lengths;
                      model.opted_lengths <- opt_length :: model.opted_lengths;

                      (* ___ If update csv signal received, update csv file ___ *)
                      if !update_csv then (
                        update_csv := false;
                        update_log_file best_lengths
                          ~missing_exp:(exp_per_config - i);
                        Printf.printf
                          "csv updated for experiment %s, check it at %s%s\n%!"
                          sim_name logs.file_path logs.file_name);
                      if !stop_experiment then raise @@ Break best_lengths)
             done;
             (* _________ Save progress _________ *)
             let diff = Unix.gettimeofday () -. !last_debug in
             if diff > 3600. then (
               debug_count := !debug_count + (int_of_float diff / 3600);
               if verbose > 0 then (
                 let hours =
                   (Unix.gettimeofday () -. start_time) /. 3600. |> int_of_float
                 in
                 Printf.printf
                   "currently testing %s, Simulation %s has been running for \
                    %d hours\n\
                    %!"
                   config sim_name hours;
                 update_log_file best_lengths;
                 last_debug := Unix.gettimeofday ()));

             best_lengths)
           []
    with Break best_lengths -> best_lengths
  in
  update_log_file best_lengths;
  Printf.printf
    "\n\nExperiment %s ended in %g seconds\nResult file available at : %s%s\n"
    sim_name
    (Unix.gettimeofday () -. start_time)
    logs.file_path logs.file_name
