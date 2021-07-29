


module IntSet = Set.Make(Int);;


module Monte_Carlo = struct


    type node_info = {visit : float; score : float; city : int; tot_dist : int; sons : mtc_node list; }
    (* le type qui contient les infos contenues dans chaque node *)

    and heritage = Root of {start_city : int; path : int list} | F of mtc_node
    (* le type qui contient la référence au node précédent *)

    and mtc_node = {info : node_info; herit : heritage}
    (* Un noeud de l'arbre de monte carlo *)

    type playout_mode = Roulette | Random
    (* Mode de selection du playout : Random correspond à une sélection aléatoire et Roulette
    à une selection pondérée par la distance au *)

    type args = {playout_mode : playout_mode;}



    let playout


end;;