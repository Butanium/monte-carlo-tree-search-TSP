module RndQ = Random_Queue
module Optimizer_2opt = Two_Opt

type edge = {
  from_city : int;
  to_city : int;
  mutable visit : float;
  mutable opt_visit : int;
  mutable score : float;
  mutable best_score : int;
  mutable best_hidden_score : int;
  cost : int;
  mutable active : bool;
}
[@@deriving show]
(** {FR} Le type contenant les caractéristiques d'une arête.
    {EN}  The type containing the characteristics of an edge. *)

(* let test_warning = 2 *)

let edge_to_string edge =
  Printf.sprintf
    "edge : %d -> %d , active : %b, opt visits/visits : %d/%.0f, best hidden \
     score : %d, best score : %d, average score : %.0f\n"
    edge.from_city edge.to_city edge.active edge.opt_visit edge.visit
    edge.best_hidden_score edge.best_score (edge.score /. edge.visit)

type heat_map = edge array array [@@deriving show]

type edge_list = edge list [@@deriving show]

(** {FR} todo 
   {EN} todo *)
type exploration_adjustment =
  | Roulette of float
  | Roulette_add_only of float
  | Roulette_min of { min_factor : float; max_bonus : float; min_bonus : float }
  | No_adjustment

(*todo add it to MCTS main*)
let str_of_exploration_adjustment = function
  | Roulette b ->
      if b = 0. then "Roulette" else Printf.sprintf "Roulette%.2fbonus" b
  | Roulette_min { min_factor; max_bonus; min_bonus } ->
      Printf.sprintf "Roulette%.2fminfactor_%.2f-%.2fbonus" min_factor max_bonus
        min_bonus
  | No_adjustment -> "No_adjustment"
  | Roulette_add_only f -> Printf.sprintf "Roulette%.2fbonus" f

(* {FR} Définie si les tours renvoyés par simulation sont convertis en arêtes ou non :
      - Dev_all pour tous
      - Dev_opt pour ceux optimisés secrètement
      - Dev_simulation pour ceux non optimisé
      - No_dev pour aucun des deux
   {EN} Define if simulation tours are converted in edges *)
type develop_simulation_policy = Dev_all | Dev_opt | Dev_simulation

let str_of_develop_policy = function
  | Dev_all -> Printf.sprintf "Dev_all"
  | Dev_opt -> Printf.sprintf "Dev_opt"
  | Dev_simulation -> Printf.sprintf "Dev_simulation"

(** {FR} Définie comment le score estimé est calculé à partir d'un arête
      - [Average] utilise la moyenne des scores du arête
      - [Best] utilise le meilleur score du arête
   {EN} define how the expected reward will be calculated in the selection formula :
      - [Average] will use the average length that the edge got in simulations
      - [Best] will use the best length that the edge got in simulations*)
type expected_length_policy = Average | Best

let str_of_expected_expected_length_policy = function
  | Average -> "Average"
  | Best -> "Best"

(** {FR} Définie la manière dont le trajet créé pendant le simulation va être optimisé :
todo update
    - [Two_opt { max_length; max_iter; max_time }] : optimisation 2-opt depuis le début du simulation jusqu'à la max_length-ième ville
      avec au maximum max_try itérations et max_time secondes
    - [Full_Two_opt { max_iter; max_time }] : optimisation complète du chemin avec du 2-opt, ne conserve pas l'intégrité du chemin
      construit pendant sélection
    {EN} Define how the path created during the simulation will be optimized :
      - [No_opt] : no optimization
      - [Two_opt { max_length; max_iter; max_time }] : 2-opt optimization from the beginning of the simulation until the max_length-th city
        with at most max_iter iterations and max_time seconds
      - [Full_Two_opt { max_iter; max_time }] : full optimization of the path, do not preserve the path built during selection *)
type optimization_policy =
  | Two_opt of { max_iter : int; max_time : float }
  | Simulated_Annealing

(* let default_opt = Two_opt { max_time = infinity; max_iter = max_int } *)

let str_of_optimization_policy = function
  | None -> ""
  | Some (Two_opt { max_iter; max_time }) ->
      Printf.sprintf "2-Opt%diter_%.0fs" max_iter max_time
  | Some Simulated_Annealing -> "Sim_Annealing"

type debug = {
  mutable score_hist : (float * int * int) list;
  mutable best_score_hist : (float * int * int) list;
  mutable generate_log_file : int;
}

(* todo implement deb *)
let deb = { score_hist = []; best_score_hist = []; generate_log_file = -1 }

type arguments = {
  (* problem constants *)
  start_time : float;
  city_count : int;
  start : int;
  adj_matrix : int array array;
  (* algorithm policies *)
  hidden_opt : optimization_policy option;
  exploration_adjustment : exploration_adjustment;
  expected_length_policy : expected_length_policy;
  optimization_policy : optimization_policy option;
  develop_simulation : bool;
  develop_opt : bool;
  exploration_constant : float;
  (* algorithm variables*)
  edges : edge array array;
  (* 1 iteration variables *)
  visited : bool array;
  mutable path_size : int;
  current_path : int array;
  best_tour : int array;
  (* algorithm results *)
  mutable best_score : int;
  mutable simulation_count : int;
  hidden_best_tour : int array;
  mutable hidden_best_score : int;
}
(** {FR} Type contenant tous les arguments qui n'auront donc pas besoin d'être passés
      dans les différentes fonctions
    {EN} Type containing all the info needed in the functions in order to avoid
      useless arguments*)

(** {FR} Référence au record qui est utilisé par toutes les fonctions
    {EN} Ref to the record that will be used by every functions *)
let arg =
  ref
    {
      edges = [||];
      hidden_opt = None;
      hidden_best_score = max_int;
      hidden_best_tour = [||];
      start_time = 0.;
      exploration_constant = -1.;
      exploration_adjustment = No_adjustment;
      visited = [||];
      city_count = -1;
      path_size = -1;
      adj_matrix = [||];
      current_path = [||];
      best_tour = [||];
      best_score = -1;
      simulation_count = 0;
      expected_length_policy = Average;
      optimization_policy = None;
      develop_opt = false;
      develop_simulation = false;
      start = 0;
    }

let reset_deb log_file =
  deb.score_hist <- [];
  deb.best_score_hist <- [];
  deb.generate_log_file <- log_file

let debug_edge oc edge = Printf.fprintf oc "%s\n" @@ edge_to_string edge

let tour_of_edges edge_list =
  let tour = Array.make !arg.city_count (-1) in
  List.iteri
    (fun i edge -> tour.(!arg.city_count - 1 - i) <- edge.to_city)
    edge_list

let optimization_tour = ref [||]

(** {FR} initialise les arrays utilisée ainsi que la seed 
    {EN} initialize the used arrays and the random seed *)
let init seed =
  let seed =
    match seed with
    | Some s -> s
    | None ->
        Random.self_init ();
        Random.int 1073741823
  in
  let arr () = Array.make !arg.city_count (-1) in
  optimization_tour := arr ();
  Random.init seed;
  seed

(** {FR} Ajoute le score au arête
    {EN} Add the score to the edge *)
let add_score score edge =
  edge.visit <- edge.visit +. 1.;
  if score < edge.best_score then edge.best_score <- score;
  edge.score <- edge.score +. float score

(** {FR} Optimise le chemin en fonction de la politique d'optimisation
    {EN} Optimize the path according to the optimization policy *)

let roulette edge = 1. /. float edge.cost

(** {FR} Actualise les arguments de arg au fur à mesure que l'on progresse dans l'arbre
    {EN} Update the arguments of arg as we progress in the tree *)
let update_arg edge =
  !arg.visited.(edge.to_city) <- true;
  !arg.current_path.(!arg.path_size) <- edge.to_city;
  !arg.path_size <- !arg.path_size + 1

(** {FR} Transforme un tour en une liste d'arêtes
    {EN} Transform a tour into a list of edges *)
let edges_from_tour tour =
  let rec aux i acc =
    if i >= !arg.city_count - 1 then acc
    else aux (i + 1) (!arg.edges.(tour.(i)).(tour.(i + 1)) :: acc)
  in
  aux 0 []

(** {FR} Actualise les arêtes en fonction du score du tour trouvé lors de la simulation
    {EN} Update the edges according to the score of the tour found during the simulation *)
let retropropagation score used_edges =
  if score < !arg.best_score then (
    !arg.best_score <- score;
    Util.copy_in_place !arg.best_tour ~model:!arg.current_path);
  List.iter (add_score score) used_edges

let optimize_simulation = function
  | Some (Two_opt { max_iter; max_time }) ->
      let _ =
        Optimizer_2opt.opt_fast ~max_iter ~max_time !arg.adj_matrix
          !arg.current_path
      in
      ()
  | None -> failwith "No optimization policy"
  | e ->
      failwith
      @@ Printf.sprintf "%s not implemented yet"
      @@ str_of_optimization_policy e

let get_optimized_length opt_policy =
  optimize_simulation opt_policy;
  Base_tsp.tour_length !arg.adj_matrix !arg.current_path

let opt_propagation () =
  let score = get_optimized_length !arg.optimization_policy in
  let edges = edges_from_tour !arg.current_path in
  retropropagation score edges

let hidden_opt () =
  let score = get_optimized_length !arg.hidden_opt in
  if score < !arg.hidden_best_score then (
    !arg.hidden_best_score <- score;
    Util.copy_in_place !arg.hidden_best_tour ~model:!arg.current_path);
  let edges = edges_from_tour !optimization_tour in
  let refresh edge =
    if score < edge.best_hidden_score then edge.best_hidden_score <- score
  in
  List.iter refresh edges

(** {FR} Renvoie la prochaine arête possible maximisant UCT
    {EN} Return the next edge maximizing UCT *)
let get_best_successor prec_edge =
  let get_w edge =
    match !arg.expected_length_policy with
    | Average -> if edge.visit <> 0. then edge.score /. edge.visit else 1.
    | Best -> float edge.best_score
  in
  let is_valid edge =
    (!arg.path_size = !arg.city_count - 1 || edge.to_city <> !arg.start)
    && (not !arg.visited.(edge.to_city))
    && edge.active
  in
  let valid_edges =
    Array.fold_left
      (fun valids edge -> if is_valid edge then edge :: valids else valids)
      []
      !arg.edges.(prec_edge.to_city)
  in
  match valid_edges with
  | [] -> None
  | _ ->
      let max_score = Util.list_max get_w valid_edges |> Option.get |> snd in
      let min_score = Util.list_min get_w valid_edges |> Option.get |> snd in
      let get_Q edge =
        if max_score <> min_score then
          (max_score -. get_w edge) /. (max_score -. min_score)
        else 0.
      in
      let max_roulette () =
        Util.list_max roulette valid_edges |> Option.get |> snd
      in
      let exp_factor, bonus =
        match !arg.exploration_adjustment with
        | No_adjustment -> ((fun _ -> 1.), fun _ -> 0.)
        | Roulette b ->
            let mr = max_roulette () in
            ( (fun edge -> roulette edge /. mr),
              fun edge -> b *. roulette edge /. mr )
        | Roulette_add_only b ->
            ((fun _ -> 1.), fun edge -> b *. roulette edge /. max_roulette ())
        | Roulette_min { min_factor; min_bonus; max_bonus } ->
            let max_roulette = max_roulette () in
            let min_roulette =
              Util.list_min roulette valid_edges |> Option.get |> snd
            in
            ( (fun edge ->
                roulette edge
                |> Util.interpolate (min_roulette, min_factor) (max_roulette, 1.)),
              fun edge ->
                roulette edge
                |> Util.interpolate (min_roulette, min_bonus)
                     (max_roulette, max_bonus) )
      in
      let get_UCT edge =
        let result =
          get_Q edge +. bonus edge
          +. !arg.exploration_constant *. exp_factor edge
             *. sqrt
                  (log (1. +. float !arg.simulation_count) /. (edge.visit +. 1.))
        in
        assert (result <> nan);
        result
      in
      Some (Util.list_max get_UCT valid_edges |> Option.get |> fst)

let simulation () =
  let rec aux prec_edge used_edges dist =
    match get_best_successor prec_edge with
    | None ->
        assert (!arg.path_size = !arg.city_count);
        (if !arg.develop_simulation then
         let score = dist + !arg.edges.(!arg.start).(prec_edge.to_city).cost in
         retropropagation score used_edges);
        if !arg.develop_opt then opt_propagation ();
        if !arg.hidden_opt <> None then hidden_opt ()
    | Some next_edge ->
        update_arg next_edge;
        aux next_edge (next_edge :: used_edges) (dist + next_edge.cost)
  in
  let first_edge = !arg.edges.(!arg.start).(!arg.start) in
  aux first_edge [ first_edge ] 0

(** {FR} Réinitialise les villes visitées et la taille du chemin à chaque fois qu'on repart de la racine de l'arbre
    {EN} Reset the visited cities and the path size each time we start from the root of the tree *)
let reset_arg () =
  !arg.path_size <- 0;
  Util.map_in_place (fun _ -> false) !arg.visited

(** {FR} Affiche les informations de l'arbre (opère récursivement sur le meilleur arête jusqu'à arriver à une feuille) 
    {EN} Display the tree information (recursively on the best edge until it reaches a leaf) *)
let debug_heat ?(oc = stdout) () =
  Util.matrix_iter
    (fun edge -> if edge.visit > 0. then debug_edge oc edge)
    !arg.edges

let create_edges ~city_count ~adj_matrix =
  let create_edge i j =
    {
      from_city = i;
      to_city = j;
      visit = 0.;
      opt_visit = 0;
      score = 0.;
      best_score = max_int;
      best_hidden_score = max_int;
      cost = adj_matrix.(i).(j);
      active = i <> j (* edge on the diagonal are not active *);
    }
  in
  Util.init_matrix city_count city_count create_edge

let verbose_message = Util.mcts_verbose_message

(** {FR} Créer développe l'arbre en gardant en mémoire le meilleur chemin emprunté durant les différents simulation
    {EN} Create and develop the tree, keeping in memory the best tour done during the simulations *)
let proceed_mcts ?(generate_log_file = -1) ?(log_files_path = "logs")
    ?((*?(debug_edges = false)*) expected_length_policy = Average)
    ?(city_config = "") ?(config_path = "tsp_instances")
    ?(exploration_adjustment = No_adjustment) ?(exploration_constant = sqrt 2.)
    ?(optimization_policy = None)
    ?((* Two_opt { max_iter = max_int; max_time = infinity } *)
    optimize_end_path = true) ?(verbose = 0) ?hidden_opt
    ?(optimize_end_path_time = infinity) ?(name = "")
    ?(develop_simulation_policy = Dev_simulation) ?(catch_SIGINT = true) ?seed
    ?(start = 0) city_count adj_matrix max_time max_simulation =
  (* ____ allow user exit with Ctrl+C sigint ____ *)
  let user_interrupt = ref false in
  if catch_SIGINT then
    Sys.set_signal Sys.sigint
      (Sys.Signal_handle (fun _ -> user_interrupt := true));

  let start_time = Unix.gettimeofday () in

  (* Initialize arg, the record containing all the information needed *)
  let arr () = Array.make city_count (-1) in
  arg :=
    {
      edges = create_edges ~city_count ~adj_matrix;
      start;
      start_time;
      exploration_constant;
      exploration_adjustment;
      visited = Array.make city_count false;
      city_count;
      path_size = 0;
      adj_matrix;
      current_path = arr ();
      best_tour = arr ();
      best_score = max_int;
      simulation_count = 0;
      expected_length_policy;
      optimization_policy;
      develop_simulation = develop_simulation_policy <> Dev_opt;
      develop_opt = develop_simulation_policy <> Dev_simulation;
      hidden_opt;
      hidden_best_tour = arr ();
      hidden_best_score = max_int;
    };
  let seed = init seed in
  reset_deb generate_log_file;

  let get_time () = Unix.gettimeofday () -. start_time in
  if verbose > 0 then Printf.printf "%s%!" verbose_message;
  (* ____________ MCTS algorithm ____________ *)
  while
    !arg.simulation_count = 0
    || !arg.simulation_count < max_simulation
       && (max_time = infinity || get_time () < max_time)
       && not !user_interrupt
  do
    !arg.simulation_count <- !arg.simulation_count + 1;
    reset_arg ();
    simulation ()
  done;

  (* _______________ Debug _______________ *)
  let debug_string = ref "" in
  let add_debug s = debug_string := !debug_string ^ s in
  let spent_time = Unix.gettimeofday () -. start_time in
  let best_score =
    if hidden_opt = None then !arg.best_score else !arg.hidden_best_score
  in
  let best_tour =
    if hidden_opt = None then !arg.best_tour
    else (
      add_debug
      @@ Printf.sprintf "%d but %d hidden score\n" !arg.best_score
           !arg.hidden_best_score;
      !arg.hidden_best_tour)
  in

  (* ________ Optimize returned path ________ *)
  let result_simulation_tour, opt_score =
    if optimize_end_path then (
      let start_time = Unix.gettimeofday () in
      let result_simulation_tour = Array.copy best_tour in
      let opted =
        Optimizer_2opt.opt_fast adj_matrix ~max_time:optimize_end_path_time
          result_simulation_tour
      in
      let opt_time = Unix.gettimeofday () -. start_time in
      let opt_score = Base_tsp.tour_length adj_matrix result_simulation_tour in
      let opt_delta = best_score - opt_score in
      add_debug
        (if opted then Printf.sprintf "Returned tour already optimized\n"
        else
          Printf.sprintf
            "Optimized returned tour in %g/%.0f seconds with %d delta \n"
            opt_time optimize_end_path_time opt_delta);
      (result_simulation_tour, opt_score))
    else (best_tour, best_score)
  in

  (* _______________ Log files _______________ *)
  let sim_name =
    Printf.sprintf "MCTS-%s-%.0fs-%s-%s" city_config spent_time
      (str_of_exploration_adjustment exploration_adjustment)
      (str_of_optimization_policy optimization_policy)
  in

  let debug_info oc =
    Printf.fprintf oc
      "\n\n\
       Simulation %s : %s\n\
       _______________START DEBUG INFO_______________\n\n"
      name sim_name;
    Printf.fprintf oc "%s" !debug_string;
    Printf.fprintf oc
      "\n\
       %d simulations in %.0f s, best score : %d, exploration constant : %.1f\n\
       random seed : %d" !arg.simulation_count spent_time best_score
      !arg.exploration_constant seed;
    Printf.fprintf oc "\nbest tour :\n";
    Base_tsp.print_tour ~oc best_tour;

    Printf.fprintf oc "\n________________END DEBUG INFO________________\n\n"
  in
  if verbose >= 0 then debug_info stdout;

  (* todo : debug edges if debug_edges then (
     print_endline "\n\n________________START DEBUG TREE_______________\n";
     debug_all_edges stdout root;
     print_endline "\n\n_________________END DEBUG TREE_______________\n";
     debug_info stdout); *)
  if generate_log_file > 0 then (
    let suffix = if name <> "" then name else sim_name in

    let file_path = Printf.sprintf "%s/%s" log_files_path suffix in
    let _ = File_log.create_dir_if_not_exist file_path in

    if generate_log_file > 2 then (
      let file = File_log.create_file ~file_path ~file_name:"all_scores" () in
      let oc =
        File_log.log_string_endline ~close:false ~file "timestamp,length"
      in
      Util.iter_rev
        (fun (t, s, hs) ->
          if hidden_opt = None then Printf.fprintf oc "%g,%d\n" t s
          else Printf.fprintf oc "%g,%d,%d\n" t s hs)
        deb.score_hist;
      close_out oc;
      let file = File_log.create_file ~file_path ~file_name:"best_scores" () in
      let oc =
        File_log.log_string_endline ~close:false ~file
          "simulation,timestamp,length"
      in
      Util.iter_rev (fun (t, p, len) ->
          if t = 0. then Printf.fprintf oc "%d,0,%d\n" p len
          else Printf.fprintf oc "%d,%g,%d\n" p t len)
      @@ (Unix.gettimeofday () -. start_time, !arg.simulation_count, best_score)
         :: deb.best_score_hist;
      close_out oc);

    if generate_log_file > 1 then (
      let file =
        File_log.create_file ~file_path ~file_name:"debug" ~extension:"txt" ()
      in
      let oc = File_log.get_oc file in
      debug_info oc;
      Base_tsp.print_error_ratio ~oc ~file_path:config_path best_tour adj_matrix
        city_config;
      Printf.fprintf oc "\n\n________________START DEBUG TREE_______________\n";
      (* todo debug edges debug_all_edges oc root; *)
      close_out oc);

    Base_tsp.create_opt_file ~file_path result_simulation_tour;
    if verbose >= 0 then
      let start = String.length "logs/" in
      Printf.printf "simulation directory for log files : %s\n"
      @@ String.sub file_path start
      @@ (String.length file_path - start));
  if verbose >= 1 then debug_heat ();
  (* ______________ Return result ______________ *)
  assert (Base_tsp.check_tour_validity best_tour);
  ((best_tour, best_score), (result_simulation_tour, opt_score), !arg.edges)
