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
(** {FR} Le type contenant les caractéristiques d'une arête.
    {EN}  The type containing the characteristics of an edge. *)

(** {FR} todo 
   {EN} todo *)
type exploration_adjustment = Roulette | No_adjustment

let str_of_exploration_adjustment = function
  | Roulette -> "Roulette"
  | No_adjustment -> "No_adjustment"

(* {FR} Définie si les tours renvoyés par playout sont convertis en arêtes ou non :
      - Dev_all pour tous
      - Dev_opt pour ceux optimisés secrètement
      - Dev_playout pour ceux non optimisé
      - No_dev pour aucun des deux
   {EN} Define if playout tours are converted in edges *)
type develop_playout_policy = Dev_all | Dev_opt | Dev_playout

let str_of_develop_policy = function
  | Dev_all -> Printf.sprintf "Dev_all"
  | Dev_opt -> Printf.sprintf "Dev_opt"
  | Dev_playout -> Printf.sprintf "Dev_playout"

(** {FR} Définie le paramètre d'exploration utilisée pour sélectionner le meilleur fils d'un arête
      - [Min_spanning_tree] utilise la longueur de l'arbre couvrant minimal
      - [Standard_deviation] utilise la valeur de l'écart type entre les scores des arêtes une fois que la racine de l'arbre
          est entièrement développé
    {EN} Define the exploration parameter used to select the best child of a edge
      - [Min_spanning_tree] use the length of the minimal spanning tree
      - [Standard_deviation] use the standard deviation of the score of all the children of the root once they are developed *)
type exploration_policy =
  | Min_spanning_tree of float
  | Standard_deviation of float

let str_of_exploration_policy = function
  | Min_spanning_tree c -> Printf.sprintf "Min_spanning_tree_%.2f" c
  | Standard_deviation c -> Printf.sprintf "Standard_deviation_%.2f" c

(** {FR} Définie comment le score estimé est calculé à partir d'un arête
      - [Average] utilise la moyenne des scores du arête
      - [Best] utilise le meilleur score du arête
   {EN} define how the expected reward will be calculated in the selection formula :
      - [Average] will use the average length that the edge got in playouts
      - [Best] will use the best length that the edge got in playouts*)
type expected_length_policy = Average | Best

let str_of_expected_expected_length_policy = function
  | Average -> "Average"
  | Best -> "Best"

(** {FR} Définie la manière dont le trajet créé pendant le playout va être optimisé :
todo update
    - [Two_opt { max_length; max_iter; max_time }] : optimisation 2-opt depuis le début du playout jusqu'à la max_length-ième ville
      avec au maximum max_try itérations et max_time secondes
    - [Full_Two_opt { max_iter; max_time }] : optimisation complète du chemin avec du 2-opt, ne conserve pas l'intégrité du chemin
      construit pendant sélection
    {EN} Define how the path created during the playout will be optimized :
      - [No_opt] : no optimization
      - [Two_opt { max_length; max_iter; max_time }] : 2-opt optimization from the beginning of the playout until the max_length-th city
        with at most max_iter iterations and max_time seconds
      - [Full_Two_opt { max_iter; max_time }] : full optimization of the path, do not preserve the path built during selection *)
type optimization_policy =
  | Two_opt of { max_iter : int; max_time : float }
  | Simulated_Annealing

let default_opt = Two_opt { max_time = infinity; max_iter = max_int }

let str_of_optimization_policy = function
  | Two_opt { max_iter; max_time } ->
      Printf.sprintf "2-Opt%diter_%.0fs" max_iter max_time
  | Simulated_Annealing -> "Sim_Annealing"

type debug = {
  mutable score_hist : (float * int * int) list;
  mutable best_score_hist : (float * int * int) list;
  mutable generate_log_file : int;
}

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
  optimization_policy : optimization_policy;
  develop_playout : bool;
  develop_opt : bool;
  mutable exploration_constant : float;
  (* algorithm variables*)
  edges : edge array array;
  mutable get_edge_score : edge -> float;
  (* 1 iteration variables *)
  visited : bool array;
  mutable path_size : int;
  current_path : int array;
  best_tour : int array;
  (* algorithm results *)
  mutable best_score : int;
  mutable playout_count : int;
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
      get_edge_score = (fun _ -> -1.);
      current_path = [||];
      best_tour = [||];
      best_score = -1;
      playout_count = 0;
      expected_length_policy = Average;
      optimization_policy = default_opt;
      develop_opt = false;
      develop_playout = false;
      start = 0;
    }

let reset_deb log_file =
  deb.score_hist <- [];
  deb.best_score_hist <- [];
  deb.generate_log_file <- log_file

let get_edge_info edge =
  Printf.sprintf
    "edge : %d -> %d , active : %b, opt visits/visits : %d/%.0f, best hidden \
     score : %d, best score : %d, average score : %.0f\n"
    edge.from_city edge.to_city edge.active edge.opt_visit edge.visit
    edge.best_hidden_score edge.best_score (edge.score /. edge.visit)

let debug_edge oc edge = Printf.fprintf oc "%s" @@ get_edge_info edge

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

(** {FR} Renvoie la fonction d'évaluation qui sera utilisée pendant la sélection
    {EN} Return the function which will return the score of a edge during selection *)
let get_edge_score_fun exploration_policy expected_length_policy
    exploration_constant_factor =
  let c =
    2.
    *.
    match exploration_policy with
    | Min_spanning_tree factor ->
        factor
        *. float
             (Prim_Alg.prim_alg
                (fun i j -> !arg.adj_matrix.(i).(j))
                !arg.city_count)
    | Standard_deviation factor ->
        let tot =
          Array.fold_left
            (fun acc arr -> acc + if arr.(0).active then 1 else 0)
            0 !arg.edges
          |> float
        in
        let sum =
          Array.fold_left
            (fun acc edge_arr ->
              let edge = edge_arr.(0) in
              acc +. if edge.active then edge.score else 0.)
            0. !arg.edges
        in
        let average = sum /. tot in
        factor
        *. (Array.fold_left
              (fun acc arr -> acc +. ((arr.(0).score -. average) ** 2.))
              0. !arg.edges
           /. tot)
           ** 0.5
  in
  !arg.exploration_constant <- c;
  let get_expected_length =
    match expected_length_policy with
    | Average -> fun edge -> edge.score /. edge.visit
    | Best -> fun edge -> float edge.best_score
  in
  (* todo introduce exploration adjustment *)
  fun edge ->
    get_expected_length edge
    -. c *. exploration_constant_factor
       *. sqrt (2. *. log (1. +. float !arg.playout_count) /. (edge.visit +. 1.))

(** {FR} Optimise le chemin en fonction de la politique d'optimisation
    {EN} Optimize the path according to the optimization policy *)
let optimize_tour container = function
  | Two_opt { max_iter; max_time } ->
      Util.copy_in_place container ~model:!arg.current_path;
      let _ =
        Optimizer_2opt.opt_fast ~max_iter ~max_time !arg.adj_matrix container
      in
      ()
  | e ->
      failwith
      @@ Printf.sprintf "%s not implemented yet"
      @@ str_of_optimization_policy e

(** {FR} Renvoie la prochaine arête possible ayant le score le plus bas
    {EN} Return the next edge with the lowest score *)
let get_best_successor prec_edge =
  Array.fold_left
    (fun ((acc_score, _) as acc) edge ->
      let score = !arg.get_edge_score edge in
      if (not !arg.visited.(edge.to_city)) && edge.active && score < acc_score
      then (score, Some edge)
      else acc)
    (infinity, None)
    !arg.edges.(prec_edge.to_city)
  |> snd

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

let optimization opt_policy =
  Util.copy_in_place !optimization_tour ~model:!arg.current_path;
  optimize_tour !optimization_tour opt_policy;
  Base_tsp.tour_length !arg.adj_matrix !optimization_tour

let opt_propagation () =
  let score = optimization !arg.optimization_policy in
  let edges = edges_from_tour !optimization_tour in
  retropropagation score edges

let hidden_opt () =
  let score = optimization @@ Option.get !arg.hidden_opt in
  if score < !arg.hidden_best_score then (
    !arg.hidden_best_score <- score;
    Util.copy_in_place !arg.hidden_best_tour ~model:!arg.current_path);
  let edges = edges_from_tour !optimization_tour in
  let refresh edge =
    if score < edge.best_hidden_score then edge.best_hidden_score <- score
  in
  List.iter refresh edges

let simulation ?(first_edge = !arg.edges.(!arg.start).(!arg.start)) () =
  let rec aux prec_edge used_edges dist =
    match get_best_successor prec_edge with
    | None ->
        assert (!arg.path_size = !arg.city_count);
        (if !arg.develop_playout then
         let score =
           dist + !arg.edges.(first_edge.from_city).(prec_edge.to_city).cost
         in
         retropropagation score used_edges);
        if !arg.develop_opt then opt_propagation ();
        if !arg.hidden_opt <> None then hidden_opt ()
    | Some next_edge ->
        aux next_edge (next_edge :: used_edges) (dist + next_edge.cost)
  in
  aux first_edge [ first_edge ] first_edge.cost

(** {FR} Réinitialise les villes visitées et la taille du chemin à chaque fois qu'on repart de la racine de l'arbre
    {EN} Reset the visited cities and the path size each time we start from the root of the tree *)
let reset_arg () =
  !arg.path_size <- 0;
  Util.map_in_place (fun _ -> false) !arg.visited

(** {FR} Affiche les informations de l'arbre (opère récursivement sur le meilleur arête jusqu'à arriver à une feuille) 
    {EN} Display the tree information (recursively on the best edge until it reaches a leaf) *)
(* let debug_all_edges oc root = ()
   todo : implement *)

let create_edges () =
  let create_edge i j =
    {
      from_city = i;
      to_city = j;
      visit = 0.;
      opt_visit = 0;
      score = 0.;
      best_score = max_int;
      best_hidden_score = max_int;
      cost = !arg.adj_matrix.(i).(j);
      active = i <> j (* edge on the diagonal are not active *);
    }
  in
  Util.init_matrix !arg.city_count !arg.city_count create_edge

let verbose_message = Util.mcts_verbose_message

(** {FR} Créer développe l'arbre en gardant en mémoire le meilleur chemin emprunté durant les différents playout
    {EN} Create and develop the tree, keeping in memory the best tour done during the playouts *)
let proceed_mcts ?(generate_log_file = -1) ?(log_files_path = "logs")
    ?((*?(debug_edges = false)*) expected_length_policy = Average)
    ?(city_config = "") ?(config_path = "tsp_instances")
    ?(exploration_adjustment = Roulette)
    ?(exploration_policy = Standard_deviation 1.)
    ?(optimization_policy = Two_opt { max_iter = max_int; max_time = infinity })
    ?(optimize_end_path = true) ?(verbose = 1) ?hidden_opt
    ?(optimize_end_path_time = infinity) ?(name = "")
    ?(develop_playout_policy = Dev_playout) ?(catch_SIGINT = true)
    ?(exploration_constant_factor = 1.) ?seed ?(start = 0) city_count adj_matrix
    max_time max_playout =
  (* ____ allow user exit with Ctrl+C sigint ____ *)
  let user_interrupt = ref false in
  if catch_SIGINT then
    Sys.set_signal Sys.sigint
      (Sys.Signal_handle (fun _ -> user_interrupt := true));

  let start_time = Unix.gettimeofday () in
  let edges : edge array array = [| [||] |] in

  (* todo creatte matrix *)

  (* Initialize arg, the record containing all the information needed *)
  let arr () = Array.make city_count (-1) in
  arg :=
    {
      edges;
      start;
      start_time;
      exploration_constant = 0.;
      exploration_adjustment;
      visited = Array.make city_count false;
      city_count;
      path_size = 0;
      adj_matrix;
      get_edge_score = (fun _ -> -1.);
      current_path = arr ();
      best_tour = arr ();
      best_score = max_int;
      playout_count = 0;
      expected_length_policy;
      optimization_policy;
      develop_playout = develop_playout_policy <> Dev_opt;
      develop_opt = develop_playout_policy <> Dev_playout;
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
    !arg.playout_count = 0
    || !arg.playout_count < max_playout
       && (max_time = infinity || get_time () < max_time)
       && not !user_interrupt
  do
    reset_arg ();
    !arg.playout_count <- !arg.playout_count + 1;
    simulation ();
    if !arg.playout_count = city_count - 1 then
      !arg.get_edge_score <-
        get_edge_score_fun exploration_policy expected_length_policy
          exploration_constant_factor
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
  let result_playout_tour, opt_score =
    if optimize_end_path then (
      let start_time = Unix.gettimeofday () in
      let result_playout_tour = Array.copy best_tour in
      let opted =
        Optimizer_2opt.opt_fast adj_matrix ~max_time:optimize_end_path_time
          result_playout_tour
      in
      let opt_time = Unix.gettimeofday () -. start_time in
      let opt_score = Base_tsp.tour_length adj_matrix result_playout_tour in
      let opt_delta = best_score - opt_score in
      add_debug
        (if opted then Printf.sprintf "Returned tour already optimized\n"
        else
          Printf.sprintf
            "Optimized returned tour in %g/%.0f seconds with %d delta \n"
            opt_time optimize_end_path_time opt_delta);
      (result_playout_tour, opt_score))
    else (best_tour, best_score)
  in

  (* _______________ Log files _______________ *)
  let sim_name =
    Printf.sprintf "MCTS-%s-%.0fs-%s-%s-%s" city_config spent_time
      (str_of_exploration_adjustment exploration_adjustment)
      (str_of_exploration_policy exploration_policy)
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
       %d playouts in %.0f s, best score : %d, exploration constant : %.1f\n\
       random seed : %d" !arg.playout_count spent_time best_score
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
          "playout,timestamp,length"
      in
      Util.iter_rev (fun (t, p, len) ->
          if t = 0. then Printf.fprintf oc "%d,0,%d\n" p len
          else Printf.fprintf oc "%d,%g,%d\n" p t len)
      @@ (Unix.gettimeofday () -. start_time, !arg.playout_count, best_score)
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

    Base_tsp.create_opt_file ~file_path result_playout_tour;
    if verbose >= 0 then
      let start = String.length "logs/" in
      Printf.printf "simulation directory for log files : %s\n"
      @@ String.sub file_path start
      @@ (String.length file_path - start));

  (* ______________ Return result ______________ *)
  assert (Base_tsp.check_tour_validity best_tour);
  ((best_tour, best_score), (result_playout_tour, opt_score), edges)
