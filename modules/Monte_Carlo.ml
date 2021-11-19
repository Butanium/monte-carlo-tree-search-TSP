module IntSet = Set.Make(Int)

type node_info = {mutable visit : float; mutable score : float; city : int; tot_dist : int; mutable childs : mtc_node list; to_expend :
    int RndQ.t}
(* [FR] le type qui contient les infos contenues dans chaque node *)
(* [EN] The information stored in a node *)

and heritage = Root | Parent of mtc_node
(* [FR] le type qui contient la r�f�rence au node pr�c�dent *)
(* [EN] Reference to the precedent node if it exists *)

and mtc_node = {info : node_info; heritage : heritage}
(* [FR] Un noeud de l'arbre de monte carlo *)
(* [EN] Represents a node in the monte carlo tree *)

type playout_selection_mode = Roulette | Random
(* [FR] Mode de selection du playout : Random correspond � une s�lection al�atoire et Roulette
� une selection pond�r�e par la distance au *)
(* [EN] selection mode for the end of a playout.
- For roulette, the probability of choosing a city 'c'
as the 'i+1' citie is pondered by 1 / distance c c_i.
- Random is just random selection *)

type exploration_constant = Min_spanning_tree | Standard_deviation
(* [FR] D�finie le param�tre d'exploration utilis�e pour s�l�ctionner le meilleur fils d'un noeud
- Min_spanning_tree utilise la longueur de l'arbre couvrant minimal
- Standard_deviation utilise la valeur de l'�cart type entre les scores des noeuds une fois que la racine de l'arbre
    est entierement expandue *)
(* [EN] Define the exploration paramater used to select the best child of a node
- Min_spanning_tree use the length of the minimal spanning tree
- Standard_deviation use the standard deviation of the score of all the childs of the root once they are developped *)

type arguments = {playout_selection_mode : playout_selection_mode; mutable visited : IntSet.t; city_count : int;
                  mutable path_size : int; eval : int -> int -> int; mutable get_node_score : mtc_node -> float;
                  current_path : int array; playout_path : int array; best_path : int array; mutable best_score : int}
(* [FR] Type contenant tous les arguments qui n'auront donc pas besoin d'�tre pass�s
dans les diff�rentes fonctions *)
(* [EN] Type containing all the info needed in the functions in order to avoid
useless arguments*)

let arg = ref {playout_selection_mode=Random; visited = IntSet.empty; city_count= -1; path_size = -1;
               eval = (fun _ _ -> -1); get_node_score = (fun _ -> -1.); current_path = [||]; best_path = [||];
               best_score = -1; playout_path = [||]}
(* [FR] R�f�rence au record qui est utilis� par toutes les fonctions *)
(* [EN] Ref to the record that will be used by every functions *)


let update_weights queue last = match !arg.playout_selection_mode with
(* [FR] Actualise les poids des diff�rentes villes par rapport � la derni�re ville choisie
pour le chemin al�atoire du playout *)
(* [EN] Update the weights in the random queue according to the last city added to the playout path *)
    | Random -> ()
    | Roulette -> RndQ.change_weights (fun _ city -> 1. /. float_of_int (!arg.eval city last)) queue


let available() =
(* [FR] Renvoie une file al�atoire contenant toutes les villes non visit�es *)
(* [EN] Returns a random queue containing all non visited cities *)
    let i = ref 0 in
    let rec aux() =
        let x = !i in
            incr i;
            if x >= !arg.city_count then raise @@ Invalid_argument "ville invalide [available]";
            if IntSet.mem x !arg.visited then aux() else x
    in
    let size = (!arg.city_count - !arg.path_size)
    in
    let cities = try (Array.init size (fun _ -> aux())) with Invalid_argument _ -> raise @@
    Invalid_argument (Printf.sprintf "%d size, %d city_count, %d path_size" size !arg.city_count !arg.path_size)
    in
    RndQ.simple_create size cities


let playout last_city start_dist =
(* [FR] Termine al�atoirement le trajet commenc� lors de l'exploration *)
(* [EN] Finish randomly the path started during the exploration *)
    let queue = available()
    in
    let size = !arg.city_count - !arg.path_size
    in
    update_weights queue last_city;
    for k = 0 to size - 1 do
        let c = RndQ.take queue in
        update_weights queue c;
        !arg.playout_path.(k) <- c
    done;
    let end_path = !arg.playout_path in
    let score = ref @@ !arg.eval last_city end_path.(0) + !arg.eval 0 end_path.(size - 1) + start_dist
    in
    for i = 1 to size - 1 do
        score := !score + !arg.eval end_path.(i-1) end_path.(i)
    done;
    if !score < !arg.best_score then (
        !arg.best_score <- !score;
        for i = 0 to !arg.path_size - 1 do
            !arg.best_path.(i) <- !arg.current_path.(i)
        done;
        for i = 0 to size - 1 do
            !arg.best_path.(!arg.path_size + i) <- end_path.(i)
        done;
    );
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
(* [FR] D�veloppe l'arbre en cr�ant un nouveau noeud reli� � 'node' *)
(* [EN] Expend the tree by adding a new node linked to 'node' *)
    let city = RndQ.take node.info.to_expend in
    !arg.visited <- IntSet.add city !arg.visited;
    !arg.current_path.(!arg.path_size) <- city;
    !arg.path_size <- !arg.path_size + 1;
    let tot_dist = node.info.tot_dist + !arg.eval node.info.city city in
    let to_expend = available() in
    let info = {visit = 0.; score = 0.; city; tot_dist; childs=[]; to_expend} in
    let new_node = {info; heritage = Parent node} in
    node.info.childs <- new_node :: node.info.childs;
    let result = float_of_int @@ playout city tot_dist in
    retropropagation new_node result


let get_node_score_fun root exploration_mode =
(* [FR] Renvoie la fonction d'�valuation qui sera utilis�e pendant la s�lection *)
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
(* [FR] Actualise les arguments de arg au fur � mesure que l'on progresse dans l'arbre *)
(* [EN] Update the arguments while exploring the tree *)
    !arg.visited <- IntSet.add node.info.city !arg.visited;
    !arg.current_path.(!arg.path_size) <- node.info.city;
    !arg.path_size <- !arg.path_size + 1


let rec selection node =
(* [FR] Parcours l'arbre en prenant le meilleur fils recursivement jusqu'� atteindre une feuille ou un noed n'ayant
 pas tous ses fils develepp�s *)
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
(* [FR] R�initialise les villes visist�es et la taille du chemin � chaque fois qu'on repart de la racine de l'arbre *)
(* [EN] Reset the visited cities and the path size every time we restart our exploration from the root *)
    !arg.path_size <- 0;
    !arg.visited <- IntSet.empty


let procede_mcts playout_selection_mode exploration_mode city_count eval max_time max_playout =
(* [FR] Cr�er d�veloppe l'abre en gardant en m�moire le meilleur chemin emprunt� durant les diff�rents playout *)
(* [EN] Create and develop the tree, keeping in memory the best path done during the playouts *)
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
    Printf.printf "\n%d playouts, %.0f s, %d length" !playout_count (Sys.time() -. start_time) !arg.best_score;
    !arg.best_path




