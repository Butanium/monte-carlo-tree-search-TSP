module RndQ = Random_Queue
module Optimizer_2opt = Two_Opt

type node_info = {
  mutable visit : float;
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

(** {FR} Politique de selection des villes lors du playout : [Random] correspond à une sélection aléatoire 
    et [Roulette] à une selection aléatoire pondérée par la distance à la dernière ville choisie
    {EN} selection policy for the end of a playout.
      - For [Roulette], the probability of choosing a city 'c'
      as the `i+1` city is pondered by `1 / distance c c_i`.
      - [Random] is just uniform random selection *)
type playout_selection_policy = Roulette | Random

let str_of_selection_policy = function
  | Roulette -> "Roulette"
  | Random -> "Random"

(* {FR} Définie si les tours renvoyés par playout sont convertis en noeuds ou non :
      - Dev_all pour tous
      - Dev_hidden pour ceux optimisés secrètement
      - Dev_playout pour ceux non optimisé
      - No_dev pour aucun des deux
   {EN} Define if playout tours are converted in nodes *)
type develop_playout_policy =
  | Dev_all of int
  | Dev_hidden of int
  | Dev_playout of int
  | No_dev

let str_of_develop_policy = function
  | Dev_all length -> Printf.sprintf "Dev_all_%d" length
  | Dev_hidden length -> Printf.sprintf "Dev_hidden%d" length
  | Dev_playout length -> Printf.sprintf "Dev_playout%d" length
  | No_dev -> "No_dev"

(** {FR} Définie le paramètre d'exploration utilisée pour sélectionner le meilleur fils d'un noeud
      - [Min_spanning_tree] utilise la longueur de l'arbre couvrant minimal
      - [Standard_deviation] utilise la valeur de l'écart type entre les scores des noeuds une fois que la racine de l'arbre
          est entièrement développé
   {EN} Define the exploration parameter used to select the best child of a node
      - [Min_spanning_tree] use the length of the minimal spanning tree
      - [Standard_deviation] use the standard deviation of the score of all the children of the root once they are developed *)
type exploration_policy = Min_spanning_tree | Standard_deviation

let str_of_exploration_policy = function
  | Min_spanning_tree -> "Min_spanning_tree"
  | Standard_deviation -> "Standard_deviation"

(** {FR} Définie comment le score estimé est calculé à partir d'un noeud
      - [Average] utilise la moyenne des scores du noeud
      - [Best] utilise le meilleur score du noeud
   {EN} define how the expected reward will be calculated in the selection formula :
      - [Average] will use the average length that the node got in playouts
      - [Best] will use the best length that the node got in playouts*)
type expected_expected_length_policy = Average | Best

let str_of_expected_expected_length_policy = function
  | Average -> "Average"
  | Best -> "Best"

(** {FR} Définie la manière dont le trajet créé pendant le playout va être optimisé :
    - [No_opt] : aucune optimisation
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
  mutable playout_time : float;
  mutable available_time : float;
  mutable pl_creation : float;
  mutable max_depth : int;
  mutable score_hist : (float * int * int) list;
  mutable best_score_hist : (float * int * int) list;
  mutable opt_time : float;
  mutable generate_log_file : int;
  mutable hidden_opt : optimization_policy;
  mutable hidden_best_tour : int array;
  mutable hidden_best_score : int;
}

let deb =
  {
    closed_nodes = 0;
    playout_time = 0.;
    available_time = 0.;
    pl_creation = 0.;
    max_depth = 0;
    score_hist = [];
    best_score_hist = [];
    opt_time = 0.;
    generate_log_file = -1;
    hidden_opt = No_opt;
    hidden_best_tour = [||];
    hidden_best_score = max_int;
  }

type arguments = {
  start_time : float;
  mutable exploration_constant : float;
  playout_selection_policy : playout_selection_policy;
  visited : bool array;
  city_count : int;
  mutable path_size : int;
  adj_matrix : int array array;
  mutable get_node_score : node -> float;
  current_path : int array;
  best_tour : int array;
  mutable best_score : int;
  mutable playout_count : int;
  expected_length_policy : expected_expected_length_policy;
  optimization_policy : optimization_policy;
  develop_playout_policy : develop_playout_policy;
  root : node option;
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
      start_time = 0.;
      exploration_constant = -1.;
      playout_selection_policy = Random;
      visited = [||];
      city_count = -1;
      path_size = -1;
      adj_matrix = [||];
      get_node_score = (fun _ -> -1.);
      current_path = [||];
      best_tour = [||];
      best_score = -1;
      playout_count = 0;
      expected_length_policy = Average;
      optimization_policy = No_opt;
      develop_playout_policy = No_dev;
      root = None;
    }

let reset_deb log_file hidden_opt =
  deb.opt_time <- 0.;
  deb.closed_nodes <- 0;
  deb.playout_time <- 0.;
  deb.available_time <- 0.;
  deb.pl_creation <- 0.;
  deb.max_depth <- 0;
  deb.score_hist <- [];
  deb.best_score_hist <- [];
  deb.generate_log_file <- log_file;
  deb.hidden_opt <- hidden_opt;
  deb.hidden_best_tour <- Array.make !arg.city_count (-1);
  deb.hidden_best_score <- max_int

let get_node_info node =
  Printf.sprintf
    "city : [%d], active : %b, visits : %.0f, best hidden score : %.0f, best \
     score : %.0f, average score : %.0f, depth : %d, max child depth : %d not \
     developed : %d\n"
    node.info.city node.info.active node.info.visit node.info.best_hidden_score
    node.info.best_score
    (node.info.score /. node.info.visit)
    node.info.depth node.info.max_child_depth
    (!arg.city_count - node.info.depth - node.info.developed)

let debug_node oc node = Printf.fprintf oc "%s" @@ get_node_info node

(** {FR} Actualise les poids des différentes villes par rapport à la dernière ville choisie
          pour le chemin aléatoire du playout
    {EN} Update the weights in the random queue according to the last city added to the playout tour *)
let update_weights queue last =
  match !arg.playout_selection_policy with
  | Random -> ()
  | Roulette -> RndQ.roulette_weights !arg.adj_matrix last queue

let creation_queue = ref @@ RndQ.simple_create 0 [||]

let playout_path = ref [||]

let r_playout_tour = ref [||]

let r_hidden_tour = ref [||]

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
  playout_path := arr ();
  r_playout_tour := arr ();
  r_hidden_tour := arr ();
  get_next_city_arr := Array.make !arg.city_count false;
  creation_queue := RndQ.simple_create !arg.city_count @@ arr ();
  Random.init seed;
  seed

(** {FR} Créer un noeud vide fils de `parent` avec la ville `city`
    {EN} Create a node with the city `city` as a child of `parent` *)
let create_node parent city =
  let depth = parent.info.depth + 1 in
  let tot_dist =
    parent.info.tot_dist + !arg.adj_matrix.(parent.info.city).(city)
  in

  let info =
    {
      visit = 0.;
      score = 0.;
      best_score = infinity;
      best_hidden_score = infinity;
      max_child_depth = depth;
      city;
      depth;
      tot_dist;
      children = [];
      developed = 0;
      active = depth <> !arg.city_count;
    }
  in
  { heritage = Parent parent; info }

(** {FR} Ajoute le score au noeud
    {EN} Add the score to the node *)
let add_score node score =
  node.info.visit <- node.info.visit +. 1.;
  if score < node.info.best_score then node.info.best_score <- score;
  node.info.score <- node.info.score +. score

(** {FR} Renvoie la fonction d'évaluation qui sera utilisée pendant la sélection
    {EN} Return the function which will return the score of a node during selection *)
let get_node_score_fun root exploration_policy expected_length_policy
    exploration_constant_factor =
  let c =
    match exploration_policy with
    | Min_spanning_tree ->
        float
        @@ Prim_Alg.prim_alg
             (fun i j -> !arg.adj_matrix.(i).(j))
             !arg.city_count
    | Standard_deviation ->
        let tot = float (!arg.city_count - 1) in
        let average =
          List.fold_left
            (fun acc node -> acc +. node.info.score)
            0. root.info.children
          /. tot
        in
        (List.fold_left
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
  let get_expected_length =
    match expected_length_policy with
    | Average -> fun node -> node.info.score /. node.info.visit
    | Best -> fun node -> node.info.best_score
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

(** {FR} remplie `store` avec le chemin construit dans la sélection puis la fin générée pendant le playout
    {EN} fill `store` with the path built during selection and the end generated during the playout *)
let fill_tour store size =
  for i = 0 to !arg.path_size - 1 do
    store.(i) <- !arg.current_path.(i)
  done;
  for i = 0 to size - 1 do
    store.(!arg.path_size + i) <- !playout_path.(i + 1)
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
          !playout_path
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
let playout last_city =
  let queue = available !creation_queue in
  let size = !arg.city_count - !arg.path_size in
  let playout_tour =
    if size > 0 then (
      update_weights queue last_city;
      !playout_path.(0) <- last_city;
      for k = 1 to size do
        let c = RndQ.take queue in
        update_weights queue c;
        !playout_path.(k) <- c
      done;
      optimize_tour !r_playout_tour size !arg.optimization_policy)
    else !arg.current_path
  in
  let playout_score = Base_tsp.tour_length !arg.adj_matrix playout_tour in

  if playout_score < !arg.best_score then (
    !arg.best_score <- playout_score;
    Util.copy_in_place !arg.best_tour playout_tour;
    if deb.generate_log_file > 0 then
      deb.best_score_hist <-
        ( Unix.gettimeofday () -. !arg.start_time,
          !arg.playout_count,
          playout_score )
        :: deb.best_score_hist);

  let hidden_score =
    if deb.hidden_opt <> No_opt && size > 0 then (
      let hidden_tour = optimize_tour !r_hidden_tour size deb.hidden_opt in
      Base_tsp.set_tour_start 0 hidden_tour;
      let opt_score = Base_tsp.tour_length !arg.adj_matrix hidden_tour in
      if opt_score < deb.hidden_best_score then (
        Util.copy_in_place deb.hidden_best_tour hidden_tour;
        deb.hidden_best_score <- opt_score);
      opt_score)
    else playout_score
  in
  if deb.generate_log_file > 0 then
    deb.score_hist <-
      (float !arg.playout_count, playout_score, hidden_score) :: deb.score_hist;

  (float playout_score, float hidden_score)

(** {FR} Actualise le nombre de visites et le score total sur les noeuds
    {EN} Update the node visited during the exploration according to the playout score 
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
  Util.copy_in_place !get_next_city_arr !arg.visited;
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
let rec forward_propagation node tour score length =
  if node.info.depth <> !arg.city_count && length >= 0 then (
    if not (node.info.city = tour.(node.info.depth - 1)) then (
      let exception T of (int array * int * int) in
      Printf.printf
        "\nError at depth %d with city %d, prev city : %d, the path is : "
        node.info.depth node.info.city
        (match node.heritage with
        | Root -> -1
        | Parent parent -> parent.info.city);
      Array.iter
        (fun c -> Printf.printf "%d " c)
        (Array.sub tour 0 @@ (1 + node.info.depth));
      raise
      @@ T (Array.sub tour 0 node.info.depth, node.info.city, node.info.depth));
    let next_city = tour.(node.info.depth) in
    match
      List.find_opt (fun x -> x.info.city = next_city) node.info.children
    with
    | None ->
        let new_node = create_node node next_city in
        add_child node new_node;
        add_score new_node score;
        forward_propagation new_node tour score (length - 1)
    | Some next_node ->
        add_score next_node score;
        forward_propagation next_node tour score (length - 1))

(** {FR} convertis le ou les tours en noeuds en fonction de la valeur de `develop_playout_policy`
    {EN} convert the tour in nodes according to the value of `develop_playout_policy` *)
let convert_tours ~hidden_score ~playout_score node =
  let root = Option.get !arg.root in
  let dev_hidden =
    add_score root hidden_score;
    forward_propagation root !r_hidden_tour hidden_score
  in
  let dev_playout =
    match !arg.optimization_policy with
    | Full_Two_opt _ -> forward_propagation root !r_playout_tour playout_score
    | _ -> forward_propagation node !r_playout_tour playout_score
  in
  let error () =
    raise
    @@ Invalid_argument
         "MCTS Error : You can't convert hidden tour if there are no hidden opt"
  in
  match !arg.develop_playout_policy with
  | No_dev -> ()
  | Dev_all extra_length ->
      if deb.hidden_opt = No_opt then error ();
      dev_playout extra_length;
      dev_hidden (extra_length + node.info.depth)
  | Dev_hidden extra_length ->
      if deb.hidden_opt = No_opt then error ();
      dev_hidden (extra_length + node.info.depth)
  | Dev_playout length -> dev_playout length

(** {FR} Développe l'arbre en créant un nouveau noeud relié à `node`
    {EN} Expand the tree by creating a new node connected to `node` *)
let expand node =
  let city =
    try get_next_city node
    with Invalid_argument e ->
      raise @@ Invalid_argument (e ^ ": get next node failed")
  in
  !arg.current_path.(!arg.path_size) <- city;
  !arg.path_size <- !arg.path_size + 1;
  assert (node.info.depth <> !arg.path_size);
  !arg.visited.(city) <- true;
  let new_node = create_node node city in
  add_child node new_node;
  let playout_score, hidden_score = playout city in
  convert_tours ~playout_score ~hidden_score node;
  backpropagation new_node playout_score hidden_score (node.info.depth + 1)

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
  deb.max_depth <- max deb.max_depth !arg.path_size;
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
            node.info.active <- false;
            deb.closed_nodes <- deb.closed_nodes + 1;
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
              let s = n.info.score /. n.info.visit in
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

(** {FR} Créer développe l'arbre en gardant en mémoire le meilleur chemin emprunté durant les différents playout
    {EN} Create and develop the tree, keeping in memory the best tour done during the playouts *)
let proceed_mcts ?(generate_log_file = -1) ?(log_files_path = "logs")
    ?(debug_tree = false) ?(expected_length_policy = Average)
    ?(city_config = "") ?(config_path = "tsp_instances")
    ?(playout_selection_policy = Roulette)
    ?(exploration_policy = Standard_deviation) ?(optimization_policy = No_opt)
    ?(stop_on_leaf = true) ?(optimize_end_path = true) ?(verbose = 1)
    ?(hidden_opt = No_opt) ?(optimize_end_path_time = infinity) ?(name = "")
    ?(develop_playout_policy = No_dev) ?(catch_SIGINT = true)
    ?(exploration_constant_factor = 1.) ?seed city_count adj_matrix max_time
    max_playout =
  let user_interrupt = ref false in
  if catch_SIGINT then
    Sys.set_signal Sys.sigint
      (Sys.Signal_handle (fun _ -> user_interrupt := true));
  (* allow user exit with Ctrl+C sigint*)
  let start_time = Unix.gettimeofday () in
  let info =
    {
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
  arg :=
    {
      start_time;
      exploration_constant = 0.;
      playout_selection_policy;
      visited = Array.make city_count false;
      city_count;
      path_size = 0;
      adj_matrix;
      get_node_score = (fun _ -> -1.);
      current_path = Array.make city_count (-1);
      best_tour = Array.make city_count (-1);
      best_score = max_int;
      playout_count = 0;
      expected_length_policy;
      optimization_policy;
      develop_playout_policy;
      root = Some root;
    };
  let seed = init seed in
  reset_deb generate_log_file hidden_opt;

  let get_time () = Unix.gettimeofday () -. start_time in
  if verbose > 0 then
    Printf.printf @@ "\n\nStarting MCTS, I'll keep informed every minutes :)\n"
    ^^ "You can stop the program at anytime by pressing Ctrl+C and it'll \
        return you its current progress \n\n"
    ^^ "    ccee88oo\n\
       \  C8O8O8Q8PoOb o8oo\n\
       \ dOB69QO8PdUOpugoO9bD\n\
        CgggbU8OU qOp qOdoUOdcb\n\
       \    6OuU  /p u gcoUodpP\n\
       \      \\\\\\//  /douUP\n\
       \        \\\\\\////\n\
       \         |||/\\\n\
       \         |||\\/\n\
       \         |:)|\n\
       \   .....//||||\\....%!\n\n";
  while
    !arg.playout_count = 0
    || !arg.playout_count < max_playout
       && (max_time = infinity || get_time () < max_time)
       && ((not stop_on_leaf) || deb.max_depth < city_count)
       && not !user_interrupt
  do
    reset_arg ();
    !arg.playout_count <- !arg.playout_count + 1;
    selection root;
    if !arg.playout_count = city_count - 1 then
      !arg.get_node_score <-
        get_node_score_fun root exploration_policy expected_length_policy
          exploration_constant_factor
  done;
  let debug_string = ref "" in
  let add_debug s = debug_string := !debug_string ^ s in
  let spent_time = Unix.gettimeofday () -. start_time in
  let best_score =
    if hidden_opt = No_opt then !arg.best_score else deb.hidden_best_score
  in
  let best_tour =
    if hidden_opt = No_opt then !arg.best_tour
    else (
      add_debug
      @@ Printf.sprintf "%d but %d hidden score\n" !arg.best_score
           deb.hidden_best_score;
      deb.hidden_best_tour)
  in
  let r_playout_tour, opt_score =
    if optimize_end_path then (
      let start_time = Unix.gettimeofday () in
      let r_playout_tour = Array.copy best_tour in
      let opted =
        Optimizer_2opt.opt_fast adj_matrix ~max_time:optimize_end_path_time
          r_playout_tour
      in
      let opt_time = Unix.gettimeofday () -. start_time in
      let opt_score = Base_tsp.tour_length adj_matrix r_playout_tour in
      let opt_delta = best_score - opt_score in
      add_debug
        (if opted then Printf.sprintf "Returned tour already optimized\n"
        else
          Printf.sprintf
            "Optimized returned tour in %g/%.0f seconds with %d delta \n"
            opt_time optimize_end_path_time opt_delta);
      (r_playout_tour, opt_score))
    else (best_tour, best_score)
  in

  let sim_name =
    Printf.sprintf "MCTS-%s-%.0fs-%s-%s-%s" city_config spent_time
      (str_of_selection_policy playout_selection_policy)
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
       %d playouts in %.0f s, max depth : %d, best score : %d, exploration \
       constant : %.1f, closed_nodes : %d\n\
       random seed : %d" !arg.playout_count spent_time deb.max_depth best_score
      !arg.exploration_constant deb.closed_nodes seed;
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

  if generate_log_file >= 0 then (
    let suffix = if name <> "" then name else sim_name in

    let file_path =
      File_log.create_log_dir @@ Printf.sprintf "%s/%s" log_files_path suffix
    in
    if generate_log_file > 0 then (
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
          "playout,timestamp,length"
      in
      Util.iter_rev (fun (t, p, len) ->
          if t = 0. then Printf.fprintf oc "%d,0,%d\n" p len
          else Printf.fprintf oc "%d,%g,%d\n" p t len)
      @@ (Unix.gettimeofday () -. start_time, !arg.playout_count, best_score)
         :: deb.best_score_hist;
      close_out oc);
    let file =
      File_log.create_file ~file_path ~file_name:"debug" ~extension:"txt" ()
    in
    let oc = File_log.get_oc file in
    debug_info oc;
    Base_tsp.print_error_ratio ~oc ~file_path:config_path best_tour adj_matrix
      city_config;
    Printf.fprintf oc "\n\n________________START DEBUG TREE_______________\n";
    debug_mcts oc root;
    close_out oc;
    Base_tsp.create_opt_file ~file_path r_playout_tour;
    if verbose >= 0 then
      let start = String.length "logs/" in
      Printf.printf "simulation directory for log files : %s\n"
      @@ String.sub file_path start
      @@ (String.length file_path - start));
  ((best_tour, best_score), (r_playout_tour, opt_score), root)
