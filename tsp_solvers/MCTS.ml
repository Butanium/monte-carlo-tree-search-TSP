module IntSet = Set.Make(Int)
module RndQ = Random_Queue 
type debug = {mutable playout_time : float; mutable available_time : float; mutable pl_creation : float;
    mutable pl_get_length_t : float; mutable pl_store : float; mutable pl_weight_update : float; mutable max_depth : int;
    mutable score_hist : (float*int) list; mutable best_score_hist : (float*int*int) list;  mutable opt_time : float; 
    mutable generate_log_file : bool}

let deb = {playout_time = 0.; available_time = 0.; pl_creation = 0.; pl_get_length_t = 0.; pl_store = 0.;
    pl_weight_update = 0.; max_depth = 0; score_hist = []; best_score_hist =[]; opt_time = 0.;
    generate_log_file = true }

let reset_deb log_file = deb.playout_time <- 0.; deb.available_time <- 0.; deb.pl_creation <- 0.; deb.pl_get_length_t <- 0.;
    deb.pl_store <- 0.; deb.pl_weight_update <- 0.; deb.max_depth <- 0; deb.score_hist <- []; deb.best_score_hist <- [];
    deb.generate_log_file <- log_file

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

type expected_expected_length_mode = Average | Best  
(* [EN] define how the expected reward will be calculated in the selection formula : 
- Average will use the average length that the node got in playouts
- Best will use the best length that the node got in playouts*)

let str_of_expected_expected_length_mode = function
    | Average -> "Average"
    | Best -> "Best"

type optimization_mode = No_opt | Two_opt of int*int*float | Simulated_Annealing
(* [FR] Définie la manière dont le trajet créer pendant le playout va être optimisé : 
    - No_opt : aucune optimisation
    - Two_opt (max_lenght, max_try, max_time) : optimisation 2-opt depuis le début du playout jusqu'à la max_length-ième ville
      avec au maximum max_try itérations et max_time secondes
*)
let str_of_optimization_mode = function
    | No_opt -> "No_optimization"
    | Two_opt (max_lenght, max_iter, max_time) -> 
        Printf.sprintf "Two_opt_optimization_%dlen_%diter_%.0fs" max_lenght max_iter max_time
    | Simulated_Annealing -> "Simulated_annealing_optimization"

type arguments = {playout_selection_mode : playout_selection_mode; mutable visited : IntSet.t; city_count : int;
                    mutable path_size : int; eval : int -> int -> int; mutable get_node_score : node -> float;
                    current_path : int array; best_path : int array; mutable best_score : int; mutable playout_count : int;
                    expected_length_mode : expected_expected_length_mode; 
                    optimization_mode : optimization_mode}
(* [FR] Type contenant tous les arguments qui n'auront donc pas besoin d'être passés
dans les différentes fonctions *)
(* [EN] Type containing all the info needed in the functions in order to avoid
useless arguments*)

let arg = ref {playout_selection_mode=Random; visited = IntSet.empty; city_count= -1; path_size = -1;
                eval = (fun _ _ -> -1); get_node_score = (fun _ -> -1.); current_path = [||]; best_path = [||];
                best_score = -1; playout_count = 0; expected_length_mode = Average; optimization_mode = No_opt}
(* [FR] Référence au record qui est utilisé par toutes les fonctions *)
(* [EN] Ref to the record that will be used by every functions *)

let get_node_info node = 
    Printf.sprintf "visits : %.0f, best score : %.0f, average score : %.0f, city : %d, depth : %d, not developed : %d\n"
        node.info.visit node.info.best_score (node.info.score /. node.info.visit) node.info.city node.info.depth (!arg.city_count - node.info.depth - node.info.developed)

let debug_node oc node = Printf.fprintf oc "%s" @@ get_node_info node    


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
    let seed = Random.int 1073741823 in 
    playout_path := Array.make !arg.city_count (-1);
    let arr = Array.make !arg.city_count (-1) in
    playout_queue := RndQ.simple_create !arg.city_count arr;
    Random.init seed; 
    seed

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

let optimize_path size = match !arg.optimization_mode with 
    | No_opt -> ()
    | Two_opt (max_lenght, max_iter, max_time) ->  
        Two_Opt.opt_fast ~partial_path:true ~upper_bound:(min size max_lenght)
          ~maxi:max_iter ~max_time !arg.eval !playout_path
    | e -> failwith @@ Printf.sprintf "%s not implemented yet" @@ str_of_optimization_mode e
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
        !playout_path.(0) <- last_city;
        for k = 1 to size do
            let c = RndQ.take queue in
            let st_wu = Sys.time() in
            update_weights queue c;
            deb.pl_weight_update <- deb.pl_weight_update +. Sys.time() -. st_wu;
            !playout_path.(k) <- c
        done;
        deb.pl_creation <- deb.pl_creation +. Sys.time() -. cr_st;
        let opt_st = Sys.time() in
        optimize_path size;
        deb.opt_time <- deb.opt_time +. Sys.time() -. opt_st;
        let end_path = !playout_path in
        let gl_st = Sys.time() in
        let score = ref @@ !arg.eval last_city end_path.(1) + !arg.eval 0 end_path.(size) + start_dist
        in
        for i = 1 to size - 1 do
            score := !score + !arg.eval end_path.(i) end_path.(i+1)
        done;
        deb.pl_get_length_t <- deb.pl_get_length_t +. Sys.time() -. gl_st;
        !score
    ) else start_dist + !arg.eval 0 last_city in 
    
    let store_st = Sys.time() in
    if score < !arg.best_score then (
        !arg.best_score <- score;
        for i = 0 to !arg.path_size - 1 do
            !arg.best_path.(i) <- !arg.current_path.(i)
        done;
        for i = 0 to size - 1 do
            !arg.best_path.(!arg.path_size + i) <- !playout_path.(i+1)
        done;
        if deb.generate_log_file then 
            deb.best_score_hist <- (Sys.time(), !arg.playout_count, score) :: deb.best_score_hist
    );
    deb.pl_store <- deb.pl_store +. Sys.time() -. store_st;
    deb.playout_time <- deb.playout_time +. Sys.time() -. st;
    if deb.generate_log_file then 
    deb.score_hist <- (Sys.time(), score) :: deb.score_hist;
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

let get_node_score_fun root exploration_mode expected_length_mode =
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
    let get_expected_length = match expected_length_mode with 
        | Average -> fun node -> node.info.score /. node.info.visit  
        | Best -> fun node -> node.info.best_score
    in
    fun node -> get_expected_length node -. 2. *. c *. sqrt (2. *. log (get_parent_visit node) /. node.info.visit)


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
    deb.max_depth <- max deb.max_depth !arg.path_size;
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


let rec debug_mcts oc root =
    Printf.fprintf oc "\n\nchosen : \n\n";
    debug_node oc root;
    Printf.fprintf oc "\nchildren : \n\n";
    match root.info.children with
    | [] -> ()
    | l -> begin
        List.iter (fun n ->
            (match n.heritage with Root -> () | Parent f ->
                Printf.fprintf oc "conv : %.1f%%  |  " @@ (100. *. n.info.visit /. f.info.visit); debug_node oc n;))
                @@ List.sort (fun n1 n2 -> -compare n1.info.visit n2.info.visit) l;
        let n,_ = List.fold_left (fun ((_, acc_s) as acc) n -> let s = n.info.score /. n.info.visit in
            if s < acc_s then n,s else acc) (root, infinity) l in
        debug_mcts oc n;

    end


let proceed_mcts 
        ?(generate_log_file=false) 
        ?(debug_tree = true) 
        ?(expected_length_mode=Average) 
        ?(city_config = "") 
        ?(playout_selection_mode=Roulette)  
        ?(exploration_mode=Standard_deviation) 
        ?(optimization_mode=No_opt)
        ?(stop_on_leaf = true)
        ?(optimize_end_path = false)
        ?(optimize_end_path_time = -.1.)
        city_count eval max_time max_playout =
(* [FR] Créer développe l'arbre en gardant en mémoire le meilleur chemin emprunté durant les différents playout *)
(* [EN] Create and develop the tree, keeping in memory the best path done during the playouts *)
    let user_interrupt = ref false in 
    Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> user_interrupt := true)); 
    (* allow user exit with Ctrl+C sigint*)
    reset_deb generate_log_file;
    arg := {playout_selection_mode; visited = IntSet.empty; city_count; path_size = 0; eval;
            get_node_score = (fun _ -> -1.); current_path = Array.make city_count (-1);
            best_path = Array.make city_count (-1); best_score = max_int; playout_count = 0; expected_length_mode;
            optimization_mode};
    let seed = init() in 
    let start_time = Sys.time() in
    let info = {visit = 0.; score = 0.; best_score = infinity; depth = 1; city = 0; tot_dist = 0; children = []; 
        developed = 0} in
    let root = {info; heritage = Root} in
    let next_debug_print = ref 60. in 
    let minutes = ref 0 in 
    let get_time() = Sys.time() -. start_time in 
    Printf.printf @@"\n\nStarting MCTS, I'll keep informed every minutes :)\n" ^^
"You can stop the program at anytime by pressing Ctrl+C and it'll return you its current progress \n\n" ^^
"    ccee88oo
  C8O8O8Q8PoOb o8oo
 dOB69QO8PdUOpugoO9bD
CgggbU8OU qOp qOdoUOdcb
    6OuU  /p u gcoUodpP
      \\\\\\//  /douUP
        \\\\\\////
         |||/\\
         |||\\/
         |:)|
   .....//||||\\....%!\n\n";
    while !arg.playout_count < max_playout && get_time() < max_time 
          && (not stop_on_leaf || deb.max_depth < city_count) && not !user_interrupt do
        if get_time() > !next_debug_print then (
            next_debug_print := 60. +. !next_debug_print;
            incr minutes;
            Printf.eprintf "\n Running for %d minutes, %d playout done | Best score : %d | Max depth : %d/%d\n%!" 
                !minutes !arg.playout_count !arg.best_score deb.max_depth city_count
        );
        reset_arg();
        !arg.playout_count <- !arg.playout_count + 1;
        selection root;
        if !arg.playout_count = city_count - 1 then
            !arg.get_node_score <- get_node_score_fun root exploration_mode expected_length_mode
    done;
    let spent_time = Sys.time() -. start_time in 
    if optimize_end_path then begin
        let start_time = Sys.time() in 
        Two_Opt.opt_fast eval ~max_time:optimize_end_path_time !arg.best_path;
        Printf.printf "Optimized returned path in %.0f/%.0f seconds\n" (Sys.time() -. start_time) (optimize_end_path_time);
    end;
    print_endline "\n\n_______________START DEBUG INFO_______________\n";
    let debug_info oc = 
        Printf.fprintf oc "\n%d playouts, %.0f s, %d max depth, best score : %d, exploration constat : %.1f, random seed : %d " !arg.playout_count spent_time deb.max_depth !arg.best_score !exploration_constant seed;
        Printf.fprintf oc ("\n%.1f playout time, %.1f available time, %.3f playout time ratio\n%.3f creation time ratio in playout,"
                ^^ " %.3f playout optimization ratio, %.3f opt ratio in playout\n") deb.playout_time deb.available_time
            (deb.playout_time /. spent_time) (deb.pl_creation /. deb.playout_time) (deb.opt_time /. spent_time) (deb.opt_time /. deb.playout_time);
        Printf.fprintf oc "weight update ratio : %.3f, get length ratio : %.3f , store ratio : %.3f \n" (deb.pl_weight_update /. deb.playout_time) (deb.pl_get_length_t
        /. deb.playout_time) (deb.pl_store /. deb.playout_time);
        Base_tsp.print_path ~oc !arg.best_path;
        Printf.fprintf oc "\n________________END DEBUG INFO________________\n\n";
    in
    debug_info stdout;
    if debug_tree then begin
        print_endline "\n\n________________START DEBUG TREE_______________\n";
        debug_mcts stdout root;
        print_endline "\n\n_________________END DEBUG TREE_______________\n";
        print_endline "\n\n______START DEBUG INFO (same as above)________\n";
        debug_info stdout
    end;
    if generate_log_file then begin
        let suffix = Printf.sprintf "MCTS-%s-%.0fs-%s-%s-%s" city_config spent_time (str_of_selection_mode playout_selection_mode) 
        (str_of_exploration_mode exploration_mode) (str_of_optimization_mode optimization_mode) in 
        let file_path = File_log.create_log_dir @@ "logs/"^suffix in 

        let file = File_log.create_file ~file_path ~file_name:"all_scores" () in 
        File_log.log_datas (fun (t, s) -> Printf.sprintf "%f,%d;" t s) file @@ List.rev deb.score_hist;

        let file = File_log.create_file ~file_path ~file_name:"best_scores" () in
        File_log.log_datas (fun (t,x,y) -> Printf.sprintf "%d,%f,%d;" x t y) file  @@ List.rev @@ (Sys.time(),!arg.playout_count, !arg.best_score) :: deb.best_score_hist;
        let file = File_log.create_file ~file_path ~file_name:"debug" () in
        let oc = File_log.get_oc file in 
        Printf.fprintf oc "\n\n_______________START DEBUG INFO_______________\n\n";
        debug_info oc;
        debug_mcts oc root;
        close_out oc;


        let start = String.length "logs/" in
        Printf.printf "simulation directory for log files : %s\n" @@ 
            String.sub file_path start @@
                String.length file_path - start;
    end;
    assert (Base_tsp.check_path_validity !arg.best_path);
    !arg.best_path, root


