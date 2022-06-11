module RndQ = Random_Queue
module Optimizer_2opt = Two_Opt

type node_info = {
  mutable visit : float;
  mutable dev_visit : int;
  mutable score : float;
  mutable best_score : float;
  mutable best_hidden_score : float;
  mutable max_child_depth : int;
  city : int;
  depth : int;
  tot_dist : int;
  mutable children : node list;
  mutable developed : int;
  mutable active : bool;
}
(** {FR} le type qui contient les infos contenues dans chaque node
    {EN} The information stored in a node *)

(** {FR} le type qui contient la référence au node précédent
   {EN} Reference to the precedent node if it exists *)
and heritage = Root | Parent of node

and node = { info : node_info; heritage : heritage }
(** {FR} Un noeud de l'arbre de monte carlo
    {EN} Represents a node in the monte carlo tree *)

(** {FR} Politique de selection des villes lors du simulation : [Random] correspond à une sélection aléatoire 
    et [Roulette] à une selection aléatoire pondérée par la distance à la dernière ville choisie
    {EN} selection policy for the end of a simulation.
      - For [Roulette], the probability of choosing a city 'c'
      as the `i+1` city is pondered by `1 / distance c c_i`.
      - [Random] is just uniform random selection *)
type simulation_selection_policy = Roulette | Random

let str_of_selection_policy = function
  | Roulette -> "Roulette"
  | Random -> "Random"

(* {FR} Définie si les tours renvoyés par simulation sont convertis en noeuds ou non :
      - Dev_all pour tous
      - Dev_hidden pour ceux optimisés secrètement
      - Dev_simulation pour ceux non optimisé
      - No_dev pour aucun des deux
   {EN} Define if simulation tours are converted in nodes *)
type develop_simulation_policy =
  | Dev_all of int
  | Dev_hidden of int
  | Dev_simulation of int
  | No_dev

let str_of_develop_policy = function
  | Dev_all length ->
      Printf.sprintf "Dev_all%s"
        (if length > 0 then Printf.sprintf "+%d" length else "")
  | Dev_hidden length -> Printf.sprintf "Dev_hidden%s" (if length > 0 then Printf.sprintf "+%d" length else "")
  | Dev_simulation length -> Printf.sprintf "Dev_simulation%s" (if length > 0 then Printf.sprintf "+%d" length else "")
  | No_dev -> "No_dev"

(** {FR} Définie le paramètre d'exploration utilisée pour sélectionner le meilleur fils d'un noeud
      - [Min_spanning_tree] utilise la longueur de l'arbre couvrant minimal
      - [Standard_deviation] utilise la valeur de l'écart type entre les scores des noeuds une fois que la racine de l'arbre
          est entièrement développé
    {EN} Define the exploration parameter used to select the best child of a node
      - [Min_spanning_tree] use the length of the minimal spanning tree
      - [Standard_deviation] use the standard deviation of the score of all the children of the root once they are developed *)
type exploration_policy =
  | Min_spanning_tree of float
  | Standard_deviation of float

let str_of_exploration_policy = function
  | Min_spanning_tree c -> Printf.sprintf "Min_spanning_tree_%.2f" c
  | Standard_deviation c -> Printf.sprintf "Standard_deviation_%.2f" c

(** {FR} Définie comment le score estimé est calculé à partir d'un noeud
      - [Average] utilise la moyenne des scores du noeud
      - [Best] utilise le meilleur score du noeud
   {EN} define how the expected reward will be calculated in the selection formula :
      - [Average] will use the average length that the node got in simulations
      - [Best] will use the best length that the node got in simulations*)
type expected_length_policy = Average | Best

let str_of_expected_length_policy = function
  | Average -> "Average"
  | Best -> "Best"

(** {FR} Définie la manière dont le trajet créé pendant le simulation va être optimisé :
    - [No_opt] : aucune optimisation
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
  | No_opt
  | Two_opt of { max_length : int; max_iter : int; max_time : float }
  | Full_Two_opt of { max_iter : int; max_time : float }
  | Simulated_Annealing

let str_of_optimization_policy = function
  | No_opt -> "No_optimization"
  | Two_opt { max_length; max_iter; max_time } ->
      Printf.sprintf "Two_opt_optimization_%dlen_%diter_%.0fs" max_length
        max_iter max_time
  | Simulated_Annealing -> "Simulated_annealing_optimization"
  | Full_Two_opt { max_iter; max_time } ->
      Printf.sprintf "Full_Two_opt_optimization_%diter_%.0fs" max_iter max_time

let str_of_optimization_policy_short = function
  | No_opt -> "No_opt"
  | Two_opt { max_length; max_iter; max_time } ->
      Printf.sprintf "2opt-%dlen_%s%.0fs" max_length
        (if max_iter = max_int then "" else Printf.sprintf "%diter_" max_iter)
        max_time
  | Simulated_Annealing -> "SimAn"
  | Full_Two_opt { max_iter; max_time } ->
      Printf.sprintf "Full2opt-%s%.0fs"
        (if max_iter = max_int then "" else Printf.sprintf "%diter_" max_iter)
        max_time

type debug = {
  mutable closed_nodes : int;
  mutable max_depth : int;
  mutable score_hist : (float * int * int) list;
  mutable best_score_hist : (float * int * int) list;
  mutable generate_log_file : int;
}

let deb =
  {
    closed_nodes = 0;
    max_depth = 0;
    score_hist = [];
    best_score_hist = [];
    generate_log_file = -1;
  }

type arguments = {
  (* Problem constants *)
  start_time : float;
  city_count : int;
  adj_matrix : int array array;
  (* Algorithm objects *)
  root : node option;
  mutable exploration_constant : float;
  mutable get_node_score : node -> float;
  (* Algorithm policies *)
  close_nodes : bool;
  hidden_opt : optimization_policy;
  simulation_selection_policy : simulation_selection_policy;
  expected_length_policy : expected_length_policy;
  optimization_policy : optimization_policy;
  develop_simulation_policy : develop_simulation_policy;
  (* 1 iteration variable *)
  visited : bool array;
  mutable path_size : int;
  current_path : int array;
  (* Algorithm results *)
  best_tour : int array;
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
      close_nodes = false;
      hidden_opt = No_opt;
      hidden_best_score = max_int;
      hidden_best_tour = [||];
      start_time = 0.;
      exploration_constant = -1.;
      simulation_selection_policy = Random;
      visited = [||];
      city_count = -1;
      path_size = -1;
      adj_matrix = [||];
      get_node_score = (fun _ -> -1.);
      current_path = [||];
      best_tour = [||];
      best_score = -1;
      simulation_count = 0;
      expected_length_policy = Average;
      optimization_policy = No_opt;
      develop_simulation_policy = No_dev;
      root = None;
    }

let reset_deb log_file =
  deb.closed_nodes <- 0;
  deb.max_depth <- 0;
  deb.score_hist <- [];
  deb.best_score_hist <- [];
  deb.generate_log_file <- log_file

let get_node_info node =
  Printf.sprintf
    "city : [%d], active : %b, dev visits/visits : %d/%.0f, best hidden score \
     : %.0f, best score : %.0f, average score : %.0f, depth : %d, max child \
     depth : %d not developed : %d\n"
    node.info.city node.info.active node.info.dev_visit node.info.visit
    node.info.best_hidden_score node.info.best_score
    (node.info.score /. node.info.visit)
    node.info.depth node.info.max_child_depth
    (!arg.city_count - node.info.depth - node.info.developed)

let debug_node oc node = Printf.fprintf oc "%s" @@ get_node_info node

(** {FR} Actualise les poids des différentes villes par rapport à la dernière ville choisie
          pour le chemin aléatoire du simulation
    {EN} Update the weights in the random queue according to the last city added to the simulation tour *)
let update_weights queue last =
  match !arg.simulation_selection_policy with
  | Random -> ()
  | Roulette -> RndQ.roulette_weights !arg.adj_matrix last queue

let creation_queue = ref @@ RndQ.simple_create 0 [||]

let simulation_path = ref [||]

let simulation_tour_container = ref [||]

let hidden_tour_container = ref [||]

let get_next_city_arr = ref [||]

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
  simulation_path := arr ();
  simulation_tour_container := arr ();
  hidden_tour_container := arr ();
  get_next_city_arr := Array.make !arg.city_count false;
  creation_queue := RndQ.simple_create !arg.city_count @@ arr ();
  Random.init seed;
  seed

(** {FR} Renvoie le chemin parcouru pour arriver au noeud en partant 
         de la racine
    {EN} Return the path from the root to the node *)
let path_of_node =
  let rec aux acc node =
    match node.heritage with
    | Parent parent -> aux (node.info.city :: acc) parent
    | Root -> node.info.city :: acc
  in
  aux []

(** {FR} Créer un noeud vide fils de `parent` avec la ville `city`
    {EN} Create a node with the city `city` as a child of `parent` *)
let create_node parent city =
  let depth = parent.info.depth + 1 in
  deb.max_depth <- max deb.max_depth depth;
  let tot_dist =
    parent.info.tot_dist + !arg.adj_matrix.(parent.info.city).(city)
  in

  let info =
    {
      visit = 0.;
      dev_visit = 0;
      score = 0.;
      best_score = infinity;
      best_hidden_score = infinity;
      max_child_depth = depth;
      city;
      depth;
      tot_dist;
      children = [];
      developed = 0;
      active = depth <> !arg.city_count || not !arg.close_nodes;
    }
  in
  { heritage = Parent parent; info }

(** {FR} Ajoute le score au noeud
    {EN} Add the score to the node *)
let add_score node score =
  node.info.visit <- node.info.visit +. 1.;
  if score < node.info.best_score then node.info.best_score <- score;
  node.info.score <- node.info.score +. score

(**
  {FR} Renvoie la longueur attendue si on choisit ce noeud
  {EN} Return the expected length of the tour if we chose this node  
*)
let get_expected_length =
  match !arg.expected_length_policy with
  | Average -> fun node -> node.info.score /. node.info.visit
  | Best -> fun node -> node.info.best_score

(** {FR} Renvoie la fonction d'évaluation qui sera utilisée pendant la sélection
    {EN} Return the function which will return the score of a node during selection *)
let get_node_score_fun root exploration_policy exploration_constant_factor =
  let c =
    match exploration_policy with
    | Min_spanning_tree factor ->
        factor
        *. float
             (Prim_Alg.prim_alg
                (fun i j -> !arg.adj_matrix.(i).(j))
                !arg.city_count)
    | Standard_deviation factor ->
        let tot = float (!arg.city_count - 1) in
        let average =
          List.fold_left
            (fun acc node -> acc +. node.info.score)
            0. root.info.children
          /. tot
        in
        factor
        *. (List.fold_left
              (fun acc node -> acc +. ((node.info.score -. average) ** 2.))
              0. root.info.children
           /. tot)
           ** 0.5
  in
  !arg.exploration_constant <- c;
  let get_parent_visit n =
    match n.heritage with
    | Root -> failwith "can't calculate score of the root"
    | Parent p -> p.info.visit
  in

  fun node ->
    get_expected_length node
    -. 2. *. c *. exploration_constant_factor
       *. sqrt (2. *. log (get_parent_visit node) /. node.info.visit)

(** {FR} Crée une fonction qui renvoie la prochaine ville non visitée toutes les fois qu'elle est appelée
    {EN} Create a function that will return the next non visited city every time it's called *)
let create_city_generator visited =
  let i = ref 0 in
  let rec aux set () =
    let x = !i in
    incr i;
    if x >= !arg.city_count then
      raise @@ Invalid_argument "ville invalide [city generator]";
    if visited.(x) then aux set () else x
  in
  aux visited

(** {FR} Renvoie une file aléatoire contenant toutes les villes non visitées
    {EN} Returns a random queue containing all non visited cities *)
let available queue =
  let aux = create_city_generator !arg.visited in
  let size = !arg.city_count - !arg.path_size in
  RndQ.reset queue;
  RndQ.set_size queue size;
  for i = 0 to size - 1 do
    RndQ.replace_element queue i (aux ())
  done;
  queue

(** {FR} remplie `container` avec le chemin construit dans la sélection puis la fin générée pendant le simulation
    {EN} fill `container` with the path built during selection and the end generated during the simulation *)
let fill_tour container size =
  for i = 0 to !arg.path_size - 1 do
    container.(i) <- !arg.current_path.(i)
  done;
  for i = 0 to size - 1 do
    container.(!arg.path_size + i) <- !simulation_path.(i + 1)
  done

(** {FR} Optimise le chemin en fonction de la politique d'optimisation
    {EN} Optimize the path according to the optimization policy *)
let optimize_tour container size = function
  | No_opt ->
      fill_tour container size;
      container
  | Two_opt { max_length; max_iter; max_time } ->
      let _ =
        Optimizer_2opt.opt_fast ~partial_path:true
          ~upper_bound:(min size max_length) ~max_iter ~max_time !arg.adj_matrix
          !simulation_path
      in
      fill_tour container size;
      container
  | Full_Two_opt { max_iter; max_time } ->
      fill_tour container size;
      let _ =
        Optimizer_2opt.opt_fast ~max_iter ~max_time !arg.adj_matrix container
      in
      container
  | e ->
      failwith
      @@ Printf.sprintf "%s not implemented yet"
      @@ str_of_optimization_policy e

(** {FR} Termine aléatoirement le chemin commencé lors de l'exploration, l'optimise si demandé
    et retourne la longueur obtenue
    {EN} Finish randomly the path started during the exploration, optimize it if asked and 
    return the computed length *)
let simulation last_city =
  let queue = available !creation_queue in
  let size = !arg.city_count - !arg.path_size in
  let simulation_tour =
    if size > 0 then (
      update_weights queue last_city;
      !simulation_path.(0) <- last_city;
      for k = 1 to size do
        let c = RndQ.take queue in
        update_weights queue c;
        !simulation_path.(k) <- c
      done;
      optimize_tour !simulation_tour_container size !arg.optimization_policy)
    else !arg.current_path
  in
  let simulation_score = Base_tsp.tour_length !arg.adj_matrix simulation_tour in

  if simulation_score < !arg.best_score then (
    !arg.best_score <- simulation_score;
    Util.copy_in_place !arg.best_tour ~model:simulation_tour;
    if deb.generate_log_file > 0 then
      deb.best_score_hist <-
        ( Unix.gettimeofday () -. !arg.start_time,
          !arg.simulation_count,
          simulation_score )
        :: deb.best_score_hist);

  let hidden_score =
    if !arg.hidden_opt <> No_opt && size > 0 then (
      let hidden_tour =
        optimize_tour !hidden_tour_container size !arg.hidden_opt
      in
      Base_tsp.set_tour_start 0 hidden_tour;
      let opt_score = Base_tsp.tour_length !arg.adj_matrix hidden_tour in
      if opt_score < !arg.hidden_best_score then (
        Util.copy_in_place !arg.hidden_best_tour ~model:hidden_tour;
        !arg.hidden_best_score <- opt_score);
      opt_score)
    else simulation_score
  in
  if deb.generate_log_file > 0 then
    deb.score_hist <-
      (float !arg.simulation_count, simulation_score, hidden_score)
      :: deb.score_hist;

  (float simulation_score, float hidden_score)

(** {FR} Actualise le nombre de visites et le score total sur les noeuds
    {EN} Update the node visited during the exploration according to the simulation score 
    @param node the current node to be updated
    @param score the score to add to the node
    @param hidden_score the hidden score to add to the node
    @param max_depth the depth of the children the propagation come from *)
let rec backpropagation node score hidden_score max_depth =
  add_score node score;
  if hidden_score < node.info.best_hidden_score then
    node.info.best_hidden_score <- hidden_score;
  if node.info.max_child_depth < max_depth then
    node.info.max_child_depth <- max_depth;
  match node.heritage with
  | Root -> ()
  | Parent parent -> backpropagation parent score hidden_score max_depth

(** {FR} sélectionne le noeud à développer
    {EN} select the node to be expanded *)
let get_next_city node =
  Util.copy_in_place !get_next_city_arr ~model:!arg.visited;
  List.iter
    (fun x -> !get_next_city_arr.(x.info.city) <- true)
    node.info.children;
  let aux = create_city_generator !get_next_city_arr in
  let size = !arg.city_count - node.info.depth - node.info.developed in
  RndQ.set_size !creation_queue size;
  for i = 0 to size - 1 do
    try RndQ.replace_element !creation_queue i (aux ())
    with Invalid_argument e ->
      raise @@ Invalid_argument (e ^ Printf.sprintf "%d size, %d i " size i)
  done;
  update_weights !creation_queue node.info.city;
  RndQ.take !creation_queue

(** {FR} ajoute `child` aux fils de `parent`
    {EN} add `child` to the children of `parent` *)
let add_child parent child =
  parent.info.children <- child :: parent.info.children;
  parent.info.developed <- parent.info.developed + 1

(** {FR} créer les noeuds associé au tour ou les actualise
    {EN} create the nodes associated to the tour or update them *)
let rec prepropagation node tour score length =
  if node.info.depth <> !arg.city_count then
    let next_city = tour.(node.info.depth) in
    match
      List.find_opt (fun x -> x.info.city = next_city) node.info.children
    with
    | None when length >= 0 ->
        let new_node = create_node node next_city in
        add_child node new_node;
        add_score new_node score;
        new_node.info.dev_visit <- new_node.info.dev_visit + 1;
        prepropagation new_node tour score (length - 1)
    | Some next_node when next_node.info.active ->
        add_score next_node score;
        next_node.info.dev_visit <- next_node.info.dev_visit + 1;
        prepropagation next_node tour score (length - 1)
    | _ -> ()

(** {FR} convertis le ou les tours en noeuds en fonction de la valeur de `develop_simulation_policy`
    {EN} convert the tour in nodes according to the value of `develop_simulation_policy` *)
let convert_tours ~hidden_score ~simulation_score node =
  let root = Option.get !arg.root in
  let dev_hidden =
    add_score root hidden_score;
    prepropagation root !hidden_tour_container hidden_score
  in
  let dev_simulation =
    match !arg.optimization_policy with
    | Full_Two_opt _ ->
        prepropagation root !simulation_tour_container simulation_score
    | _ -> prepropagation node !simulation_tour_container simulation_score
  in
  let error () =
    raise
    @@ Invalid_argument
         "MCTS Error : You can't convert hidden tour if there are no hidden opt"
  in
  match !arg.develop_simulation_policy with
  | No_dev -> ()
  | Dev_all extra_length ->
      if !arg.hidden_opt = No_opt then error ();
      dev_simulation extra_length;
      dev_hidden (extra_length + node.info.depth)
  | Dev_hidden extra_length ->
      if !arg.hidden_opt = No_opt then error ();
      dev_hidden (extra_length + node.info.depth)
  | Dev_simulation length -> dev_simulation length

(** {FR} Développe l'arbre en créant un nouveau noeud relié à `node`
    {EN} Expand the tree by creating a new node connected to `node` *)
let expand node =
  let city = get_next_city node in
  !arg.current_path.(!arg.path_size) <- city;
  !arg.path_size <- !arg.path_size + 1;
  assert (node.info.depth + 1 = !arg.path_size);
  !arg.visited.(city) <- true;
  let new_node = create_node node city in
  add_child node new_node;
  let simulation_score, hidden_score = simulation city in
  convert_tours ~simulation_score ~hidden_score new_node;
  backpropagation new_node simulation_score hidden_score (node.info.depth + 1)

(** {FR} Renvoie le fils de `node` ayant le score le plus bas
    {EN} Return the child of `node` with the lowest score *)
let get_best_child node =
  let rec aux acc_score acc_node = function
    | [] -> acc_node
    | n :: ns ->
        if n.info.active then
          let s = !arg.get_node_score n in
          aux (min s acc_score) (if s < acc_score then Some n else acc_node) ns
        else aux acc_score acc_node ns
  in
  match node.info.children with
  | [] -> failwith "no child found"
  | nodes -> aux infinity None nodes

(** {FR} Actualise les arguments de arg au fur à mesure que l'on progresse dans l'arbre
    {EN} Update the arguments of arg as we progress in the tree *)
let update_arg node =
  !arg.visited.(node.info.city) <- true;
  !arg.current_path.(!arg.path_size) <- node.info.city;
  !arg.path_size <- !arg.path_size + 1

(** {FR} Parcours l'arbre en prenant le meilleur fils récursivement jusqu'à atteindre une feuille ou un noeud n'ayant
           pas tous ses fils développés, et expand ce noeud là
    {EN} Recursive tree traversal, chosing the best child at each step until a leaf or a node without all its children
        is reached, and expand this node there *)
let rec selection node =
  update_arg node;
  if node.info.developed + node.info.depth = !arg.city_count then
    match node.info.children with
    | [] ->
        let dist =
          node.info.tot_dist
          + !arg.adj_matrix.(node.info.city).(!arg.current_path.(0))
        in
        if dist < !arg.best_score then (
          !arg.best_score <- dist;
          for i = 0 to !arg.city_count - 1 do
            !arg.best_tour.(i) <- !arg.current_path.(i)
          done);
        backpropagation node (float dist) (float dist) node.info.depth
    | _ -> (
        match get_best_child node with
        | Some child -> selection child
        | None ->
            if !arg.close_nodes then (
              node.info.active <- false;
              deb.closed_nodes <- deb.closed_nodes + 1);
            backpropagation node node.info.best_score
              node.info.best_hidden_score node.info.max_child_depth)
  else
    try expand node
    with Invalid_argument e ->
      raise
      @@ Invalid_argument
           (e ^ " at expansion of " ^ get_node_info node ^ "developed : "
           ^ string_of_int node.info.developed)

(** {FR} Réinitialise les villes visitées et la taille du chemin à chaque fois qu'on repart de la racine de l'arbre
    {EN} Reset the visited cities and the path size each time we start from the root of the tree *)
let reset_arg () =
  !arg.path_size <- 0;
  Util.map_in_place (fun _ -> false) !arg.visited

(** {FR} Affiche les informations de l'arbre (opère récursivement sur le meilleur noeud jusqu'à arriver à une feuille) 
    {EN} Display the tree information (recursively on the best node until it reaches a leaf) *)
let debug_mcts oc root =
  Printf.fprintf oc "\n\nRoot : \n\n";
  debug_node oc root;
  let rec aux node =
    Printf.fprintf oc "\nChildren : \n\n";
    match node.info.children with
    | [] -> ()
    | l ->
        List.iter (fun n ->
            match n.heritage with
            | Root -> ()
            | Parent f ->
                Printf.fprintf oc "conv : %.1f%%  |  "
                @@ (100. *. n.info.visit /. f.info.visit);
                debug_node oc n)
        @@ List.sort (fun n1 n2 -> -compare n1.info.visit n2.info.visit) l;
        let n, _ =
          List.fold_left
            (fun ((_, acc_s) as acc) n ->
              let s = get_expected_length node in
              if s < acc_s then (n, s) else acc)
            (node, infinity) l
        in
        (match l with
        | [ _ ] -> ()
        | _ ->
            Printf.fprintf oc "\n\nChosen : \n\n";
            debug_node oc n);
        aux n
  in
  aux root

let verbose_message = Util.mcts_verbose_message

(* TODO : refactor arguments with a record as argumennt for optional values *)

(** {FR} Créer développe l'arbre en gardant en mémoire le meilleur chemin emprunté durant les différents simulation
    {EN} Create and develop the tree, keeping in memory the best tour done during the simulations *)
let proceed_mcts ?((* Policy arguments *)
                 expected_length_policy = Average) ?(close_nodes = true)
    ?(simulation_selection_policy = Roulette)
    ?(exploration_policy = Standard_deviation 1.)
    ?(optimization_policy = No_opt) ?(stop_on_leaf = true)
    ?(optimize_end_path = true) ?(hidden_opt = No_opt)
    ?(optimize_end_path_time = infinity) ?(develop_simulation_policy = No_dev)
    ?(exploration_constant_factor = 1.) ?seed
    ?((* Problem arguments *)
    city_config = "") ?(config_path = "tsp_instances") ~city_count ~adj_matrix
    ?((* debug arguments *)
    generate_log_file = -1) ?(log_files_path = "logs") ?(debug_tree = false)
    ?(verbose = 1) ?(catch_SIGINT = true) ?(name = "")
    (* Algorithm exit conditions*)
      max_time max_simulation =
  (* ____ allow user exit with Ctrl+C sigint ____ *)
  let user_interrupt = ref false in
  if catch_SIGINT then
    Sys.set_signal Sys.sigint
      (Sys.Signal_handle (fun _ -> user_interrupt := true));

  let start_time = Unix.gettimeofday () in

  (* ______ initialize root node ______ *)
  let info =
    {
      dev_visit = 0;
      visit = 0.;
      score = 0.;
      best_score = infinity;
      best_hidden_score = infinity;
      max_child_depth = 1;
      depth = 1;
      city = 0;
      tot_dist = 0;
      children = [];
      developed = 0;
      active = true;
    }
  in
  let root = { info; heritage = Root } in

  (* Initialize arg, the record containing all the information needed *)
  let arr () = Array.make city_count (-1) in
  arg :=
    {
      close_nodes;
      start_time;
      exploration_constant = 0.;
      simulation_selection_policy;
      visited = Array.make city_count false;
      city_count;
      path_size = 0;
      adj_matrix;
      get_node_score = (fun _ -> -1.);
      current_path = arr ();
      best_tour = arr ();
      best_score = max_int;
      simulation_count = 0;
      expected_length_policy;
      optimization_policy;
      develop_simulation_policy;
      root = Some root;
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
       && ((not stop_on_leaf) || deb.max_depth < city_count)
       && not !user_interrupt
  do
    reset_arg ();
    !arg.simulation_count <- !arg.simulation_count + 1;
    selection root;
    if !arg.simulation_count = city_count - 1 then
      !arg.get_node_score <-
        get_node_score_fun root exploration_policy exploration_constant_factor
  done;

  (* _______________ Debug _______________ *)
  let debug_string = ref "" in
  let add_debug s = debug_string := !debug_string ^ s in
  let spent_time = Unix.gettimeofday () -. start_time in
  let best_score =
    if hidden_opt = No_opt then !arg.best_score else !arg.hidden_best_score
  in
  let best_tour =
    if hidden_opt = No_opt then !arg.best_tour
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
    Printf.sprintf "MCTS-%s-%.0fs-%s-%s-%s" city_config spent_time
      (str_of_selection_policy simulation_selection_policy)
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
       %d simulations in %.0f s, max depth : %d, best score : %d, exploration \
       constant : %.1f, closed_nodes : %d\n\
       random seed : %d" !arg.simulation_count spent_time deb.max_depth
      best_score !arg.exploration_constant deb.closed_nodes seed;
    Printf.fprintf oc "\nbest tour :\n";
    Base_tsp.print_tour ~oc best_tour;

    Printf.fprintf oc "\n________________END DEBUG INFO________________\n\n"
  in
  if verbose >= 0 then debug_info stdout;
  if debug_tree then (
    print_endline "\n\n________________START DEBUG TREE_______________\n";
    debug_mcts stdout root;
    print_endline "\n\n_________________END DEBUG TREE_______________\n";
    debug_info stdout);

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
          if hidden_opt = No_opt then Printf.fprintf oc "%g,%d\n" t s
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
      debug_mcts oc root;
      close_out oc);

    Base_tsp.create_opt_file ~file_path result_simulation_tour;
    if verbose >= 0 then
      let start = String.length "logs/" in
      Printf.printf "simulation directory for log files : %s\n"
      @@ String.sub file_path start
      @@ (String.length file_path - start));

  (* ______________ Return result ______________ *)
  assert (Base_tsp.check_tour_validity best_tour);
  ((best_tour, best_score), (result_simulation_tour, opt_score), root)
