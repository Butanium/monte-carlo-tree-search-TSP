module IntSet = Set.Make(Int)
module RndQ = Random_Queue 
type debug = {mutable playout_time : float; mutable available_time : float; mutable pl_creation : float;
mutable pl_get_length_t : float; mutable pl_store : float; mutable pl_weight_update : float; mutable max_depth : int;
mutable score_hist : int list; mutable best_score_hist : (int*int) list}

let deb = {playout_time = 0.; available_time = 0.; pl_creation = 0.; pl_get_length_t = 0.; pl_store = 0.;
    pl_weight_update = 0.; max_depth = 0; score_hist = []; best_score_hist =[]}

let reset_deb() = deb.playout_time <- 0.; deb.available_time <- 0.; deb.pl_creation <- 0.; deb.pl_get_length_t <- 0.;
    deb.pl_store <- 0.; deb.pl_weight_update <- 0.; deb.max_depth <- 0; deb.score_hist <- []; deb.best_score_hist <- []

type node_info = {mutable visit : float; mutable score : float; mutable best_score : float; city : int; depth : int; tot_dist : int; mutable children : node list; 
    mutable developed : int}
(* [FR] le type qui contient les infos contenues dans chaque node *)
(* [EN] The information stored in a node *)

and heritage = Root | Parent of node
(* [FR] le type qui contient la référence au node précédent *)
(* [EN] Reference to the precedent node if it exists *)

and node = {info : node_info; heritage : heritage}
(* [FR] Un noeud de l'arbre de monte carlo *)
(* [EN] Represents a node in the monte carlo tree *)



type playout_selection_mode = Roulette | Random
(* [FR] Mode de selection du playout : Random correspond é une sélection aléatoire et Roulette
é une selection pondérée par la distance au *)
(* [EN] selection mode for the end of a playout.
- For roulette, the probability of choosing a city 'c'
as the 'i+1' city is pondered by 1 / distance c c_i.
- Random is just random selection *)
let str_of_selection_mode = function 
    | Roulette -> "Roulette"
    | Random -> "Random"

type exploration_mode = Min_spanning_tree | Standard_deviation
(* [FR] Définie le paramètre d'exploration utilisée pour sélectionner le meilleur fils d'un noeud
- Min_spanning_tree utilise la longueur de l'arbre couvrant minimal
- Standard_deviation utilise la valeur de l'écart type entre les scores des noeuds une fois que la racine de l'arbre
    est entièrement développé *)
(* [EN] Define the exploration parameter used to select the best child of a node
- Min_spanning_tree use the length of the minimal spanning tree
- Standard_deviation use the standard deviation of the score of all the children of the root once they are developed *)
let str_of_exploration_mode = function 
    | Min_spanning_tree -> "Min_spanning_tree"
    | Standard_deviation -> "Standard_deviation"

type arguments = {playout_selection_mode : playout_selection_mode; mutable visited : IntSet.t; city_count : int;
                    mutable path_size : int; eval : int -> int -> int; mutable get_node_score : node -> float;
                    current_path : int array; best_path : int array; mutable best_score : int; mutable playout_count : int}
(* [FR] Type contenant tous les arguments qui n'auront donc pas besoin d'être passés
dans les différentes fonctions *)
(* [EN] Type containing all the info needed in the functions in order to avoid
useless arguments*)

let arg = ref {playout_selection_mode=Random; visited = IntSet.empty; city_count= -1; path_size = -1;
                eval = (fun _ _ -> -1); get_node_score = (fun _ -> -1.); current_path = [||]; best_path = [||];
                best_score = -1; playout_count = 0}
(* [FR] Référence au record qui est utilisé par toutes les fonctions *)
(* [EN] Ref to the record that will be used by every functions *)

let get_node_info node = 
    Printf.sprintf "visits : %.0f, best score : %.1f, average score : %.1f, city : %d, depth : %d, not developed : %d\n"
        node.info.visit node.info.best_score (node.info.score /. node.info.visit) node.info.city node.info.depth (!arg.city_count - node.info.depth - node.info.developed)

let debug_node node = print_string @@ get_node_info node    


let update_weights queue last = match !arg.playout_selection_mode with
(* [FR] Actualise les poids des différentes villes par rapport é la dernière ville choisie
pour le chemin aléatoire du playout *)
(* [EN] Update the weights in the random queue according to the last city added to the playout path *)
    | Random -> ()
    | Roulette -> RndQ.change_weights (fun _ city -> 1. /. float_of_int (!arg.eval city last)) queue

let playout_queue = ref @@ RndQ.simple_create 0 [||]

let playout_path = ref [||]




let init() =
    Random.self_init();
    playout_path := Array.make !arg.city_count (-1);
    let arr = Array.make !arg.city_count (-1) in
    playout_queue := RndQ.simple_create !arg.city_count arr

let create_city_generator set = 
    let i = ref 0 in 
    let rec aux set () =
        let x = !i in
            incr i;
            if x >= !arg.city_count then raise @@ Invalid_argument "ville invalide [available]";
            if IntSet.mem x set then aux set () else x
    in aux set
let available queue =
(* [FR] Renvoie une file aléatoire contenant toutes les villes non visitées *)
(* [EN] Returns a random queue containing all non visited cities *)
    let st = Sys.time() in
    let aux = create_city_generator !arg.visited in 
    let size = (!arg.city_count - !arg.path_size)
    in
    RndQ.reset queue;
    RndQ.set_size queue size;
    for i=0 to size-1 do 
        RndQ.replace_element queue i (aux())
    done;
     (* try ()) with Invalid_argument _ -> raise @@
    Invalid_argument (Printf.sprintf "%d size, %d city_count, %d path_size" size !arg.city_count !arg.path_size) *)
    
    deb.available_time <- deb.available_time +. Sys.time() -. st;
    queue

let get_next_node node = 
    let set = List.fold_left (fun acc x -> IntSet.add x.info.city acc) !arg.visited node.info.children in 
    let aux = create_city_generator set in  
    let size = !arg.city_count - node.info.depth - node.info.developed in 
    RndQ.set_size !playout_queue 1;
    for i = 0 to size - 1 do 
        try (
        RndQ.replace_element !playout_queue i (aux ())
        ) with Invalid_argument e -> raise @@ Invalid_argument (e ^ Printf.sprintf "%d size, %d i " size i)
    done;
    update_weights !playout_queue node.info.city;
    RndQ.take !playout_queue

let playout last_city start_dist =
(* [FR] Termine aléatoirement le trajet commencé lors de l'exploration *)
(* [EN] Finish randomly the path started during the exploration *)
    let st = Sys.time() in
    let queue = available !playout_queue
    in
    let size = !arg.city_count - !arg.path_size
    in
    let score = 
    if size > 0 then (
        update_weights queue last_city;
        let cr_st = Sys.time() in
        for k = 0 to size - 1 do
            let c = RndQ.take queue in
            let st_wu = Sys.time() in
            update_weights queue c;
            deb.pl_weight_update <- deb.pl_weight_update +. Sys.time() -. st_wu;
            !playout_path.(k) <- c
        done;
        deb.pl_creation <- deb.pl_creation +. Sys.time() -. cr_st;
        let end_path = !playout_path in
        let gl_st = Sys.time() in
        let score = ref @@ !arg.eval last_city end_path.(0) + !arg.eval 0 end_path.(size - 1) + start_dist
        in
        for i = 1 to size - 1 do
            score := !score + !arg.eval end_path.(i-1) end_path.(i)
        done;
        deb.pl_get_length_t <- deb.pl_get_length_t +. Sys.time() -. gl_st;
        !score) else start_dist + !arg.eval 0 last_city in 
    
    let store_st = Sys.time() in
    if score < !arg.best_score then (
        !arg.best_score <- score;
        for i = 0 to !arg.path_size - 1 do
            !arg.best_path.(i) <- !arg.current_path.(i)
        done;
        for i = 0 to size - 1 do
            !arg.best_path.(!arg.path_size + i) <- !playout_path.(i)
        done;
        deb.best_score_hist <- (!arg.playout_count, score) :: deb.best_score_hist
    );
    deb.pl_store <- deb.pl_store +. Sys.time() -. store_st;
    deb.playout_time <- deb.playout_time +. Sys.time() -. st;
    deb.score_hist <- score :: deb.score_hist;
    score


let rec retropropagation node value =
(* [FR] Actualise le nombre de visite et le score total sur les noeuds *)
(* [EN] Update the node visited during the exploration according to the playout score *)
    node.info.visit <- node.info.visit +. 1.;
    node.info.score <- node.info.score +. value;
    if value < node.info.best_score then (
        node.info.best_score <- value;
    );
    match node.heritage with
        | Root -> ()
        | Parent parent -> retropropagation parent value


let expand node =
(* [FR] Développe l'arbre en créant un nouveau noeud relié é 'node' *)
(* [EN] Expand the tree by adding a new node linked to 'node' *)
    let city = try(
        get_next_node node
     ) with Invalid_argument e -> raise @@ Invalid_argument (e ^ ": get next node failed") in 
    !arg.current_path.(!arg.path_size) <- city;
    !arg.path_size <- !arg.path_size + 1;
    !arg.visited <- IntSet.add city !arg.visited;
    deb.max_depth <- max deb.max_depth !arg.path_size;
    let depth = node.info.depth + 1 in 
    let tot_dist = node.info.tot_dist + !arg.eval node.info.city city in
    let info = {visit = 0.; score = 0.; best_score = infinity; city; depth; tot_dist; children=[]; developed=0} in
    let new_node = {info; heritage = Parent node} in
    node.info.children <- new_node :: node.info.children;
    node.info.developed <- node.info.developed + 1;
    let result = try (float_of_int @@ playout city tot_dist) with Invalid_argument e -> raise @@ 
        Invalid_argument (Printf.sprintf "playout failed with path_size %d : " !arg.path_size ^ e) in
    retropropagation new_node result

let exploration_constant = ref @@ -1.

let get_node_score_fun root exploration_mode =
(* [FR] Renvoie la fonction d'évaluation qui sera utilisée pendant la sélection *)
(* [EN] Return the function which will return the score of a node during selection *)
    let c = (match exploration_mode with
        | Min_spanning_tree -> float_of_int @@ Prim_Alg.prim_alg !arg.eval !arg.city_count
        | Standard_deviation ->
            let tot = float_of_int ( !arg.city_count - 1 ) in
            let average = List.fold_left (fun acc node -> acc +. node.info.score) 0. root.info.children /. tot in
            (List.fold_left (fun acc node -> acc +. (node.info.score -. average) ** 2.) 0. root.info.children /. tot) ** 0.5
    ) in
    exploration_constant := c;
    let get_parent_visit n = match n.heritage with
        | Root -> failwith "can't calculate score of the root"
        | Parent p -> p.info.visit
    in
    fun node -> let average_node_score = node.info.score /. node.info.visit in
                    average_node_score -. 2. *. c *. sqrt (2. *. log (get_parent_visit node) /. node.info.visit)


let get_best_child node =
(* [FR] Renvoie le fils de `node` ayant le score le plus bas *)
(* [EN] Returns the child of `node` having the lowest score *)
    let rec aux acc_score acc_node = function
        | [] -> acc_node
        | n :: ns -> let s = !arg.get_node_score n in
                        aux (min s acc_score) (if s < acc_score then n else acc_node) ns
    in
    match node.info.children with
        | [] -> failwith "no child found"
        | n :: ns -> aux (!arg.get_node_score n) n ns

let update_arg node =
(* [FR] Actualise les arguments de arg au fur é mesure que l'on progresse dans l'arbre *)
(* [EN] Update the arguments while exploring the tree *)
    !arg.visited <- IntSet.add node.info.city !arg.visited;
    !arg.current_path.(!arg.path_size) <- node.info.city;
    !arg.path_size <- !arg.path_size + 1


let rec selection node =
(* [FR] Parcours l'arbre en prenant le meilleur fils récursivement jusqu'à atteindre une feuille ou un noeud n'ayant
    pas tous ses fils développés *)
(* [EN] Browse through the tree, picking the best child recursively until it reaches a leaf
or a node with undeveloped children *)
    update_arg node;
    if node.info.developed + node.info.depth = !arg.city_count then
        match node.info.children with
            | [] -> let dist = node.info.tot_dist + !arg.eval node.info.city !arg.current_path.(0) in
                if dist < !arg.best_score then (
                    !arg.best_score <- dist;
                    for i = 0 to !arg.city_count - 1 do
                        !arg.best_path.(i) <- !arg.current_path.(i)
                    done
                );
                retropropagation node @@ float_of_int dist
            | _ -> selection @@ get_best_child node
    else
        try (expand node) with Invalid_argument e -> raise @@ Invalid_argument (e ^ " at expansion of " ^ get_node_info node ^ "develped : " ^ string_of_int node.info.developed)


let reset_arg() =
(* [FR] Réinitialise les villes visitées et la taille du chemin é chaque fois qu'on repart de la racine de l'arbre *)
(* [EN] Reset the visited cities and the path size every time we restart our exploration from the root *)
    !arg.path_size <- 0;
    !arg.visited <- IntSet.empty


let rec debug_mcts root =
    print_endline "\nchosen : \n";
    debug_node root;
    print_endline "\nroot children : \n";
    match root.info.children with
    | [] -> ()
    | l -> begin
        List.iter (fun n ->
            (match n.heritage with Root -> () | Parent f ->
                Printf.printf "conv : %.1f%%  |  " @@ (100. *. n.info.visit /. f.info.visit); debug_node n;))
                @@ List.sort (fun n1 n2 -> -compare n1.info.visit n2.info.visit) l;
        let n,_ = List.fold_left (fun ((_, acc_s) as acc) n -> let s = n.info.score /. n.info.visit in
            if s < acc_s then n,s else acc) (root, infinity) l in
        debug_mcts n;

    end


let proceed_mcts ?(debug_tree = true) ?(city_config = "")playout_selection_mode exploration_mode city_count eval max_time max_playout =
(* [FR] Créer développe l'arbre en gardant en mémoire le meilleur chemin emprunté durant les différents playout *)
(* [EN] Create and develop the tree, keeping in memory the best path done during the playouts *)
    reset_deb();
    arg := {playout_selection_mode; visited = IntSet.empty; city_count; path_size = 0; eval;
            get_node_score = (fun _ -> -1.); current_path = Array.make city_count (-1);
            best_path = Array.make city_count (-1); best_score = max_int; playout_count = 0};
    init();

    let start_time = Sys.time() in
    let info = try ({visit = 0.; score = 0.; best_score = infinity; depth = 1; city = 0; tot_dist = 0; children = []; 
        developed = 0}) with Invalid_argument _ -> failwith "info" in
    let root = {info; heritage = Root} in


    while !arg.playout_count < max_playout && Sys.time() -. start_time < max_time do
        reset_arg();
        !arg.playout_count <- !arg.playout_count + 1;
        selection root;
        if !arg.playout_count = city_count - 1 then
            !arg.get_node_score <- get_node_score_fun root exploration_mode
    done;
    let spent_time = Sys.time() -. start_time in 
    print_endline "\n\n_______________START DEBUG INFO_______________\n";
    let debug_info() = 
        Printf.printf "\n%d playouts, %.0f s, %d max depth, best score : %d, exploration constat : %.1f " !arg.playout_count spent_time deb.max_depth !arg.best_score !exploration_constant;
        Printf.printf "\n%.1f playout time, %.1f available time, %.3f playout time ratio\n %.3f creation time ratio in playout \n" deb.playout_time deb.available_time
            (deb.playout_time /. (Sys.time() -. start_time)) (deb.pl_creation /. deb.playout_time);
        Printf.printf "weight update ratio : %.3f, get length ratio : %.3f , store ratio : %.3f \n" (deb.pl_weight_update /. deb.playout_time) (deb.pl_get_length_t
        /. deb.playout_time) (deb.pl_store /. deb.playout_time);
        print_endline   "\n________________END DEBUG INFO________________\n";
    in
    debug_info(); 
    if debug_tree then begin
        print_endline "\n\n________________START DEBUG TREE_______________\n";
        debug_mcts root;
        print_endline "\n\n_________________END DEBUG TREE_______________\n";
        print_endline "\n\n______START DEBUG INFO (same as above)________\n";
        debug_info()
    end;
    let suffix = Printf.sprintf "-%s-%.0fs-%s-%s-%d_playouts" city_config spent_time (str_of_selection_mode playout_selection_mode)
    (str_of_exploration_mode exploration_mode) (!arg.playout_count) in 
    let file_path, file_name = "logs/score_logs", "all_scores" ^ suffix in 
    let file = File_log.create_file ~file_path ~file_name () in 
    File_log.log_datas (fun x -> Printf.sprintf "%d," x) file @@ List.rev deb.score_hist;
    let file_path, file_name = "logs/best_score_logs", "best_scores" ^ suffix in 
    let file = File_log.create_file ~file_path ~file_name () in 
    File_log.log_datas (fun (x,y) -> Printf.sprintf "%d,%d;" x y) file  @@ List.rev @@ (!arg.playout_count, !arg.best_score) :: deb.best_score_hist;

    !arg.best_path

