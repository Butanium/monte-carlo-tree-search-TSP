module Monte_Carlo = struct
    (* Same as Monte_Carlo.ml but with some more debug features *)
    type debug = {mutable playout_time : float; mutable available_time : float; mutable pl_creation : float;
    mutable pl_getlent : float; mutable pl_store : float; mutable pl_weight_update : float; mutable max_depth : int;
    mutable score_hist : int list; mutable best_score_hist : int*int list}

    let deb = {playout_time = 0.; available_time = 0.; pl_creation = 0.; pl_getlent = 0.; pl_store = 0.;
        pl_weight_update = 0.; max_depth = 0; score_hist = []; best_score_hist =[]}

    let reset_deb() = deb.playout_time <- 0.; deb.available_time <- 0.; deb.pl_creation <- 0.; deb.pl_getlent <- 0.;
        deb.pl_store <- 0.; deb.pl_weight_update <- 0.; deb.max_depth <- 0; deb.score_hist <- []; deb.best_score_hist <- []

    type node_info = {mutable visit : float; mutable score : float; city : int; tot_dist : int; mutable childs : mtc_node list; to_expend :
        int RndQ.t}
    (* [FR] le type qui contient les infos contenues dans chaque node *)
    (* [EN] The information stored in a node *)

    and heritage = Root | Parent of mtc_node
    (* [FR] le type qui contient la référence au node précédent *)
    (* [EN] Reference to the precedent node if it exists *)

    and mtc_node = {info : node_info; heritage : heritage}
    (* [FR] Un noeud de l'arbre de monte carlo *)
    (* [EN] Represents a node in the monte carlo tree *)

    type playout_selection_mode = Roulette | Random
    (* [FR] Mode de selection du playout : Random correspond à une sélection aléatoire et Roulette
    à une selection pondérée par la distance au *)
    (* [EN] selection mode for the end of a playout.
    - For roulette, the probability of choosing a city 'c'
    as the 'i+1' citie is pondered by 1 / distance c c_i.
    - Random is just random selection *)

    type exploration_constant = Min_spanning_tree | Standard_deviation
    (* [FR] Définie le paramètre d'exploration utilisée pour séléctionner le meilleur fils d'un noeud
    - Min_spanning_tree utilise la longueur de l'arbre couvrant minimal
    - Standard_deviation utilise la valeur de l'écart type entre les scores des noeuds une fois que la racine de l'arbre
        est entierement expandue *)
    (* [EN] Define the exploration paramater used to select the best child of a node
    - Min_spanning_tree use the length of the minimal spanning tree
    - Standard_deviation use the standard deviation of the score of all the childs of the root once they are developped *)

    type arguments = {playout_selection_mode : playout_selection_mode; mutable visited : IntSet.t; city_count : int;
                      mutable path_size : int; eval : int -> int -> int; mutable get_node_score : mtc_node -> float;
                      current_path : int array; playout_path : int array; best_path : int array; mutable best_score : int}
    (* [FR] Type contenant tous les arguments qui n'auront donc pas besoin d'être passés
    dans les différentes fonctions *)
    (* [EN] Type containing all the info needed in the functions in order to avoid
    useless arguments*)

    let arg = ref {playout_selection_mode=Random; visited = IntSet.empty; city_count= -1; path_size = -1;
                   eval = (fun _ _ -> -1); get_node_score = (fun _ -> -1.); current_path = [||]; best_path = [||];
                   best_score = -1; playout_path = [||]}
    (* [FR] Référence au record qui est utilisé par toutes les fonctions *)
    (* [EN] Ref to the record that will be used by every functions *)


    let update_weights queue last = match !arg.playout_selection_mode with
    (* [FR] Actualise les poids des différentes villes par rapport à la dernière ville choisie
    pour le chemin aléatoire du playout *)
    (* [EN] Update the weights in the random queue according to the last city added to the playout path *)
        | Random -> ()
        | Roulette -> RndQ.change_weights (fun _ city -> 1. /. float_of_int (!arg.eval city last)) queue


    let available() =
    (* [FR] Renvoie une file aléatoire contenant toutes les villes non visitées *)
    (* [EN] Returns a random queue containing all non visited cities *)
        let st = Sys.time() in
        let i = ref 0 in
        let rec aux() =
            let x = !i in
                incr i;
                if x >= !arg.city_count then raise @@ Invalid_argument "ville invalide [available]";
                if IntSet.mem x !arg.visited then aux() else x
        in
        let size = (!arg.city_count - !arg.path_size)
        in
        let cities = try (Array.init size (fun _ -> aux())) with Invalid_argument e -> raise @@
        Invalid_argument (Printf.sprintf "%d size, %d city_count, %d path_size" size !arg.city_count !arg.path_size)
        in
        deb.available_time <- deb.available_time +. Sys.time() -. st;
        RndQ.simple_create size cities


    let playout last_city start_dist =
    (* [FR] Termine aléatoirement le trajet commencé lors de l'exploration *)
    (* [EN] Finish randomly the path started during the exploration *)
        let st = Sys.time() in
        let queue = available()
        in
        let size = !arg.city_count - !arg.path_size
        in
        update_weights queue last_city;
        let cr_st = Sys.time() in
        for k = 0 to size - 1 do
            let c = RndQ.take queue in
            let st_wu = Sys.time() in
            update_weights queue c;
            deb.pl_weight_update <- deb.pl_weight_update +. Sys.time() -. st_wu;
            !arg.playout_path.(k) <- c
        done;
        deb.pl_creation <- deb.pl_creation +. Sys.time() -. cr_st;
        let end_path = !arg.playout_path in
        let gl_st = Sys.time() in
        let score = ref @@ !arg.eval last_city end_path.(0) + !arg.eval 0 end_path.(size - 1) + start_dist
        in
        for i = 1 to size - 1 do
            score := !score + !arg.eval end_path.(i-1) end_path.(i)
        done;
        deb.pl_getlent <- deb.pl_getlent +. Sys.time() -.gl_st;
        let store_st = Sys.time() in
        if !score < !arg.best_score then (
            !arg.best_score <- !score;
            for i = 0 to !arg.path_size - 1 do
                !arg.best_path.(i) <- !arg.current_path.(i)
            done;
            for i = 0 to size - 1 do
                !arg.best_path.(!arg.path_size + i) <- end_path.(i)
            done;
        );
        deb.pl_store <- deb.pl_store +. Sys.time() -. store_st;
        deb.playout_time <- deb.playout_time +. Sys.time() -. st;
        !score


    let rec retropropagation node value =
    (* [FR] Actualise le nombre de visite et le score total sur les noeuds *)
    (* [EN] Update the node visited during the exploration according to the playout score *)
        node.info.visit <- node.info.visit +. 1.;
        node.info.score <- node.info.score +. value;
        match node.heritage with
            | Root -> ()
            | Parent parent -> retropropagation parent value


    let expend node =
    (* [FR] Développe l'arbre en créant un nouveau noeud relié à 'node' *)
    (* [EN] Expend the tree by adding a new node linked to 'node' *)
        let city = RndQ.take node.info.to_expend in
        !arg.visited <- IntSet.add city !arg.visited;
        !arg.current_path.(!arg.path_size) <- city;
        !arg.path_size <- !arg.path_size + 1;
        deb.max_depth <- max deb.max_depth !arg.path_size;
        let tot_dist = node.info.tot_dist + !arg.eval node.info.city city in
        let to_expend = available() in
        let info = {visit = 0.; score = 0.; city; tot_dist; childs=[]; to_expend} in
        let new_node = {info; heritage = Parent node} in
        node.info.childs <- new_node :: node.info.childs;
        let result = float_of_int @@ playout city tot_dist in
        retropropagation new_node result


    let get_node_score_fun root exploration_mode =
    (* [FR] Renvoie la fonction d'évaluation qui sera utilisée pendant la sélection *)
    (* [EN] Return the function which will return the score of a node during selection *)
        let c = (match exploration_mode with
            | Min_spanning_tree -> float_of_int @@ Primalg.primalg !arg.eval !arg.city_count
            | Standard_deviation ->
                let tot = float_of_int ( !arg.city_count - 1 ) in
                let average = List.fold_left (fun acc node -> acc +. node.info.score) 0. root.info.childs /. tot in
                (List.fold_left (fun acc node -> acc +. (node.info.score -. average) ** 2.) 0. root.info.childs /. tot) ** 0.5
        ) in
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
        match node.info.childs with
            | [] -> failwith "no child found"
            | n :: ns -> aux (!arg.get_node_score n) n ns

    let update_arg node =
    (* [FR] Actualise les arguments de arg au fur à mesure que l'on progresse dans l'arbre *)
    (* [EN] Update the arguments while exploring the tree *)
        !arg.visited <- IntSet.add node.info.city !arg.visited;
        !arg.current_path.(!arg.path_size) <- node.info.city;
        !arg.path_size <- !arg.path_size + 1


    let rec selection node =
    (* [FR] Parcours l'arbre en prenant le meilleur fils recursivement jusqu'à atteindre une feuille ou un noed n'ayant
     pas tous ses fils develeppés *)
    (* [EN] Browse through the tree, picking the best child recursively until it reachs a leaf
    or a node with undeveloped children *)
        update_arg node;
        if RndQ.is_empty node.info.to_expend then
            match node.info.childs with
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
            expend node


    let reset_arg() =
    (* [FR] Réinitialise les villes visistées et la taille du chemin à chaque fois qu'on repart de la racine de l'arbre *)
    (* [EN] Reset the visited cities and the path size every time we restart our exploration from the root *)
        !arg.path_size <- 0;
        !arg.visited <- IntSet.empty

    let debug_node node =
        Printf.printf "visits : %.0f, dist ratio : %.1f, city : %d, not developped : %d\n"
            node.info.visit (node.info.score /. node.info.visit) node.info.city node.info.to_expend.size

    let rec debug_mcts root =
        print_endline "\nchosen : \n";
        debug_node root;
        print_endline "\nroot childs : \n";
        match root.info.childs with
        | [] -> ()
        | l -> begin
            List.iter (fun n ->
                (match n.heritage with Root _ -> () | Parent f ->
                    Printf.printf "conv : %.1f%%  |  " @@ (100. *. n.info.visit /. f.info.visit); debug_node n;))
                    @@ List.sort (fun n1 n2 -> -compare n1.info.visit n2.info.visit) l;
            let n,_ = List.fold_left (fun ((acc_n, acc_s) as acc) n -> let s = n.info.score /. n.info.visit in
                if s < acc_s then n,s else acc) (root, infinity) l in
            debug_mcts n;

        end


    let procede_mcts playout_selection_mode exploration_mode city_count eval max_time max_playout =
    (* [FR] Créer développe l'abre en gardant en mémoire le meilleur chemin emprunté durant les différents playout *)
    (* [EN] Create and develop the tree, keeping in memory the best path done during the playouts *)
        reset_deb();
        arg := {playout_selection_mode; visited = IntSet.empty; city_count; path_size = 0; eval;
                get_node_score = (fun _ -> -1.); current_path = Array.make city_count (-1);
                best_path = Array.make city_count (-1); playout_path = Array.make city_count (-1); best_score = max_int};
        let playout_count = ref 0 in
        let start_time = Sys.time() in
        let info = try ({visit = 0.; score = 0.; city = 0; tot_dist = 0; childs = [];
            to_expend = RndQ.simple_create (city_count - 1) @@ Array.init (city_count - 1) (fun x -> x + 1)}) with
             Invalid_argument _ -> failwith "info" in
        let root = {info; heritage = Root} in
        while !playout_count < max_playout && Sys.time() -. start_time < max_time do
            reset_arg();
            incr playout_count;
            selection root;
            if !playout_count = city_count - 1 then
                !arg.get_node_score <- get_node_score_fun root exploration_mode
        done;

        print_endline "\n\n _______________START DEBUG INFO_______________\n";
        Printf.printf "\n%d playouts, %.0f s, %d max depth, best score : %d " !playout_count (Sys.time() -. start_time) deb.max_depth !arg.best_score;
        Printf.printf "\n%.1f playout time, %.1f available time, %.3f playout time ratio\n %.3f creation time ratio in playout \n" deb.playout_time deb.available_time
            (deb.playout_time /. (Sys.time() -. start_time)) (deb.pl_creation /. deb.playout_time);
        Printf.printf "weight update ratio : %.3f, get lenght ratio : %.3f , store ratio : %.3f \n" (deb.pl_weight_update /. deb.playout_time) (deb.pl_getlent
        /. deb.playout_time) (deb.pl_store /. deb.playout_time);
        print_endline "\n________________END DEBUG INFO_________________\n";
        print_endline "\n\n_________________START DEBUG TREE_______________\n";
        debug_mcts root;
        print_endline "\n\n_________________END DEBUG TREE_______________\n";
        !arg.best_path
end;;

