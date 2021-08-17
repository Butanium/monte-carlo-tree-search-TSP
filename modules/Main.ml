#load "graphics.cma";; module IntSet = Set.Make(Int);;
module RndQ = struct
(* Module permettant de cr�er une file al�atoire : chaque �l�ment
� un certain poids qui d�termine la probabilit� qu'il a d'�tre tir� *)

    exception Empty

    type 'a t = {mutable size: int; content : ('a * float) array; mutable tot : float}
    (** [size], la taille actuelle de la file, [content] une array dont les [size] premiers �l�ments
    repr�sentent les �l�ments restant dans la file avec leur probabilit�, [tot] est la somme des probabilit�s*)

    let simple_create size arr =
        {size; content = Array.map (fun x -> x, 1.) arr; tot = float_of_int size}
    (* Cr�er une file al�atoire contenant les [size] premiers �l�ments de [arr], o� tous les �l�ments
    ont les m�mes chances de sortir *)

    let create size arr weights = let tot = Array.fold_left (+.) 0. weights in
        {size; content = Array.mapi (fun i x -> x, weights.(i)) arr; tot}
    (* Cr�er une file al�atoire contenant les [size] premiers �l�ments de [arr] o� les chances de sortir
    d'un �l�ment de arr est pond�r� par l'�l�ment de m�me index de [weights] *)

    let is_empty q = q.size = 0
    (* Renvoie true si la file est vide *)

    let get_length q = q.size
    (* Renvoie la taille de la file *)

    let take q =
    (* Selectionne al�atoirement un �l�ment *)
        if q.size = 0 then raise Empty else
        let rec aux k acc = if k >= q.size - 1 then k else (
            let acc = acc -. let _, p = q.content.(k) in p in
                    if acc < 1e-10 then k else aux (k+1) acc
                )
        in
        let i = aux  0 @@ Random.float 1. *. q.tot  in
        (* [i] l'index selectionn� al�atoirement *)
            let res, p as r = q.content.(i) in
                q.content.(i) <- q.content.(q.size - 1);
                (* l'�l�ment i s�lectionn� est remplac� par le dernier �l�ment de la file *)
                q.content.(q.size - 1) <- r;
                (* On conserve l'�l�ment selectionn� dans l'array mais il ne fait plus partie de la file.
                On le conserve afin de pouvoir r�utiliser la file si besoin *)
                q.size <- q.size - 1;
                (* taille r�duite de 1 *)
                q.tot <- q.tot -. p;
                (* Poids total r�duit de la probabilit� de l'�l�ment choisi *)
                res

    let tot_empty q =
        Array.init q.size (fun _ -> take q)
    (* Cr�er une array contenant les �l�ments restant dans la file, dans un ordre al�atoire. *)

    let change_weights f q =
        let tot = ref 0. in
        for i = 0 to q.size - 1 do
            let x, w = q.content.(i)
            in
            let new_w = f w x in
                q.content.(i) <- (x, new_w);
                tot := !tot +. new_w
        done;
        q.tot <- !tot
    (* change les poids des diff�rents �l�ments selon f *)

    let reset q =
    (* Remet tous les �l�ments d�j� tir�s dans la file *)
        q.size <- Array.length q.content;
        q.tot <- Array.fold_left (fun acc (_,w) -> acc +. w) 0. q.content

end;;
module Readertsp = struct
    let open_tsp tsp_name =
        let city_count = Scanf.sscanf tsp_name "%[^0-9]%d" (fun _ c -> c)
        in
        let cities = Array.make city_count (0., 0.)
        in
        let fill = let i = ref 0 in
            fun x -> cities.(!i) <- x; incr i
        in
        let ic = open_in  @@
            Printf.sprintf "C:/Users/Clement/Documents/prepa/tipe/ocaml-tsp/tsp/%s.tsp" tsp_name
        in
        let rec loop started = try (let s = String.trim @@ input_line ic in
            if started then (
                let x,y = Scanf.sscanf s "%d %f %f" (fun _ x y -> (x, y))
                in
                fill (x,y);
                loop true
            ) else loop ("NODE_COORD_SECTION" = s)
            ) with _ -> ();
        in loop false;
        city_count, cities

    let open_path tsp_name =
        let city_count = Scanf.sscanf tsp_name "%[^0-9]%d" (fun _ c -> c)
        in
        let path = Array.make city_count 0
        in
        let fill = let i = ref 0 in
            fun x -> path.(!i) <- x-1; incr i
        in
        let ic = open_in  @@
            Printf.sprintf "C:/Users/Clement/Documents/prepa/tipe/ocaml-tsp/tsp/%s.opt.tour" tsp_name
        in
        let rec loop started = try (let s = String.trim @@ input_line ic in
            if started then (
                List.iter fill @@ List.map int_of_string @@ String.split_on_char ' ' s;
               loop true
            ) else loop ("TOUR_SECTION" = s)
            ) with _ -> ();
        in loop false;
        path

end;;
module Basetsp = struct
    let dists cities =
        let dist (c1x,c1y) (c2x,c2y) =
            int_of_float (0.5 +. sqrt ((c1x -. c2x)*.(c1x -. c2x) +. (c1y -. c2y)*. (c1y -. c2y)))
        in
        let city_count = Array.length cities in
        let adj_matrix = Array.init city_count (fun i -> Array.init city_count (fun j -> dist cities.(i) cities.(j)))
        in
        fun c1 c2 -> adj_matrix.(c1).(c2)
    let path_length eval path =
        let s = ref 0 in
        for i = 0 to Array.length path - 2 do
            s := !s + eval path.(i) path.(i+1)
        done;
        !s + eval path.(0) path.(Array.length path - 1)

    let best_path_length config eval =
        let path = Readertsp.open_path config in
        path_length eval path
end;;
module Showtsp = struct
    open Graphics
    type parameters = {mutable height: int; mutable width: int; mutable city_size: int}

    let params = {height=600; width=600; city_size=10}

    let coordToScreen (maxX, maxY) (x,y) =
           let a,b = float_of_int params.width *. 0.1 +. 0.8 *. float_of_int params.width *. x /. maxX,
            float_of_int params.height *. 0.1 +. 0.8 *. float_of_int params.height *. y /. maxY
        in int_of_float a, int_of_float b

    let show_cities cities =
        open_graph @@ Printf.sprintf "%dx%d" params.width params.height;
        clear_graph();
        set_line_width 1;
        let maxX, maxY = Array.fold_left (fun (maxX, maxY) (x,y) -> (max maxX x), (max maxY y)) (0.,0.) cities in
        Array.iteri (fun i (x,y) ->  set_color red; fill_circle x y params.city_size;set_text_size 20;set_color black;
                    moveto x y; draw_string @@ string_of_int i) @@ Array.map (coordToScreen (maxX, maxY)) cities

    let show_solution cities sol =
        let maxX, maxY = Array.fold_left (fun (maxX, maxY) (x,y) -> (max maxX x), (max maxY y)) (0.,0.) cities
        in
        let movetoT (x,y)= moveto x y
        in
        let coord = coordToScreen (maxX, maxY)
        in
        let lineto_city city = let x,y = coord cities.(city) in lineto x y
        in
        show_cities cities;
        set_line_width 3;
        set_color black;
        movetoT @@ coord cities.(sol.(0)) ;
        for k = 1 to Array.length sol - 1 do
            lineto_city sol.(k);
        done;
        lineto_city sol.(0)
        
    let show_solution_list cities sol =
        let maxX, maxY = Array.fold_left (fun (maxX, maxY) (x,y) -> (max maxX x), (max maxY y)) (0.,0.) cities
        in
        let movetoT (x,y)= moveto x y
        in
        let coord = coordToScreen (maxX, maxY)
        in
        let lineto_city city = let x,y = coord cities.(city) in lineto x y
        in
        show_cities cities;
        set_line_width 3;
        set_color black;
        let x :: xs = sol in 
            
        movetoT @@ coord cities.(x);
        List.iter lineto_city xs;
        lineto_city x
    let show_best_path config =
        let _, cities = Readertsp.open_tsp config in
        show_solution cities (Readertsp.open_path config)
    
end;;
module TwoOpt = struct
    let invertPath i j path =
        for k = 0 to (j - i)/2 - 1 do
             let t = path.(i+1+k) in
             path.(i+1+k) <- path.(j-k);
             path.(j-k) <- t;
         done
    let opt_best ?(debug = false) ?(maxi = -1) eval path =
        let bound = Array.length path in

        let rec loop k =
            let diff = ref 0 in
            let minI, minJ = ref 0, ref 0 in
                for i = 0 to bound - 4 do
                    for j = i+2 to bound - 1 - max 0 (1-i) do
                        let d = eval path.(i) path.(j) + eval path.(i+1) path.((j+1) mod bound)
                        - eval path.(i) path.(i+1) - eval path.(j) path.((j+1) mod bound)
                        in
                        if d < !diff then (
                            diff := d;
                            minI := i;
                            minJ := j
                        )
                    done
                done;
            if !diff < 0 then (
                invertPath !minI !minJ path;
                if debug then Printf.printf "\ninverted %d and %d" !minI !minJ;
                if k < maxi || maxi < 0 then loop (k+1)
            )
        in loop 1

    let opt_fast ?(debug = false) ?(maxi = -1) eval path =
        let bound = Array.length path in
        let rec rec_while i = (i < maxi || maxi < 0) &&
            not (loop1 0) && rec_while (i+1)
        and loop1 i = i >= bound - 4 || (loop2 i (i+2) && loop1 (i+1))
        and loop2 i j = j >= bound - max 0 (1-i)  || (
            let diff = eval path.(i) path.(j) + eval path.(i+1) path.((j+1) mod bound)
                                   - eval path.(i) path.(i+1) - eval path.(j) path.((j+1) mod bound)  in
            if diff < 0 then (
                invertPath i j path;
                if debug then Printf.printf "\ninverted %d and %d, diff : %d" i j diff;
                false
            ) else true
        ) && loop2 i (j+1)
        in
        let _ = rec_while 0 in ()

    type random_creation = Roulette | Random

    let weight_update eval last q = function
        | Random -> ()
        | Roulette -> RndQ.change_weights (fun _ x -> 1. /. float_of_int(eval x last)) q

    let random_path q eval mode city_count =
        Array.init city_count (
            fun _ -> let v = RndQ.take q in
                weight_update eval v q mode;
                v
        )

    let iter_two_opt n eval city_count rnd_mode =
        let arr = Array.init city_count (Fun.id) in
        let q = RndQ.simple_create city_count arr in
        let best_len = ref max_int in
        let best_path = Array.make city_count (-1) in
        for _ = 1 to n do
            let path = random_path q eval rnd_mode city_count in
            opt_fast eval path;
            let len = Basetsp.path_length eval path in
            if len < !best_len then (
                best_len := len;
                for i = 0 to city_count -1 do
                    best_path.(i) <- path.(i)
                done
            );
            RndQ.reset q

        done;
        best_path

end;;
module Primalg = struct
    let primalg eval city_count =
        let init_visited = IntSet.singleton 0 in
        let init_visit =
            let rec aux1 i acc =
            if i < city_count then aux1 (i + 1) (IntSet.add i acc) else acc
            in aux1 1 IntSet.empty
        in
        let rec aux to_visit visited score =
            if not @@ IntSet.is_empty to_visit then (
                let added = ref (-1) in
                let mini = ref max_int in
                IntSet.iter (fun c1 ->
                    IntSet.iter (fun c2 ->
                        let len = eval c1 c2 in
                        if len < !mini then (
                            mini := len;
                            added := c1
                        )) visited) to_visit;
                let new_to_visit = IntSet.remove !added to_visit
                in
                let new_visited = IntSet.add !added visited
                in
                aux new_to_visit new_visited (score +  !mini)
            ) else (
                score
            )
        in
        aux init_visit init_visited 0
end;;

module Readertsp = struct
    let open_tsp tsp_name =
        let city_count = Scanf.sscanf tsp_name "%[^0-9]%d" (fun _ c -> c)
        in
        let cities = Array.make city_count (0., 0.)
        in
        let fill = let i = ref 0 in
            fun x -> cities.(!i) <- x; incr i
        in
        let ic = open_in  @@
            Printf.sprintf "C:/Users/Clement/Documents/prepa/tipe/ocaml-tsp/tsp/%s.tsp" tsp_name
        in
        let rec loop started = try (let s = String.trim @@ input_line ic in
            if started then (
                let x,y = Scanf.sscanf s "%d %f %f" (fun _ x y -> (x, y))
                in
                fill (x,y);
                loop true
            ) else loop ("NODE_COORD_SECTION" = s)
            ) with _ -> ();
        in loop false;
        city_count, cities

    let open_path tsp_name =
        let city_count = Scanf.sscanf tsp_name "%[^0-9]%d" (fun _ c -> c)
        in
        let path = Array.make city_count 0
        in
        let fill = let i = ref 0 in
            fun x -> path.(!i) <- x-1; incr i
        in
        let ic = open_in  @@
            Printf.sprintf "C:/Users/Clement/Documents/prepa/tipe/ocaml-tsp/tsp/%s.opt.tour" tsp_name
        in
        let rec loop started = try (let s = String.trim @@ input_line ic in
            if started then (
                List.iter fill @@ List.map int_of_string @@ String.split_on_char ' ' s;
               loop true
            ) else loop ("TOUR_SECTION" = s)
            ) with _ -> ();
        in loop false;
        path

end;;
module Basetsp = struct
    let dists cities =
        let dist (c1x,c1y) (c2x,c2y) =
            int_of_float (0.5 +. sqrt ((c1x -. c2x)*.(c1x -. c2x) +. (c1y -. c2y)*. (c1y -. c2y)))
        in
        let city_count = Array.length cities in
        let adj_matrix = Array.init city_count (fun i -> Array.init city_count (fun j -> dist cities.(i) cities.(j)))
        in
        fun c1 c2 -> adj_matrix.(c1).(c2)
    let path_length eval path =
        let s = ref 0 in
        for i = 0 to Array.length path - 2 do
            s := !s + eval path.(i) path.(i+1)
        done;
        !s + eval path.(0) path.(Array.length path - 1)

    let best_path_length config eval =
        let path = Readertsp.open_path config in
        path_length eval path
end;;
module Showtsp = struct
    open Graphics
    type parameters = {mutable height: int; mutable width: int; mutable city_size: int}

    let params = {height=600; width=600; city_size=10}

    let coordToScreen (maxX, maxY) (x,y) =
           let a,b = float_of_int params.width *. 0.1 +. 0.8 *. float_of_int params.width *. x /. maxX,
            float_of_int params.height *. 0.1 +. 0.8 *. float_of_int params.height *. y /. maxY
        in int_of_float a, int_of_float b

    let show_cities cities =
        open_graph @@ Printf.sprintf "%dx%d" params.width params.height;
        clear_graph();
        set_line_width 1;
        let maxX, maxY = Array.fold_left (fun (maxX, maxY) (x,y) -> (max maxX x), (max maxY y)) (0.,0.) cities in
        Array.iteri (fun i (x,y) ->  set_color red; fill_circle x y params.city_size;set_text_size 20;set_color black;
                    moveto x y; draw_string @@ string_of_int i) @@ Array.map (coordToScreen (maxX, maxY)) cities

    let show_solution cities sol =
        let maxX, maxY = Array.fold_left (fun (maxX, maxY) (x,y) -> (max maxX x), (max maxY y)) (0.,0.) cities
        in
        let movetoT (x,y)= moveto x y
        in
        let coord = coordToScreen (maxX, maxY)
        in
        let lineto_city city = let x,y = coord cities.(city) in lineto x y
        in
        show_cities cities;
        set_line_width 3;
        set_color black;
        movetoT @@ coord cities.(sol.(0)) ;
        for k = 1 to Array.length sol - 1 do
            lineto_city sol.(k);
        done;
        lineto_city sol.(0)
        
    let show_solution_list cities sol =
        let maxX, maxY = Array.fold_left (fun (maxX, maxY) (x,y) -> (max maxX x), (max maxY y)) (0.,0.) cities
        in
        let movetoT (x,y)= moveto x y
        in
        let coord = coordToScreen (maxX, maxY)
        in
        let lineto_city city = let x,y = coord cities.(city) in lineto x y
        in
        show_cities cities;
        set_line_width 3;
        set_color black;
        let x :: xs = sol in 
            
        movetoT @@ coord cities.(x);
        List.iter lineto_city xs;
        lineto_city x
    let show_best_path config =
        let _, cities = Readertsp.open_tsp config in
        show_solution cities (Readertsp.open_path config)
    
end;;
module TwoOpt = struct
    let invertPath i j path =
        for k = 0 to (j - i)/2 - 1 do
             let t = path.(i+1+k) in
             path.(i+1+k) <- path.(j-k);
             path.(j-k) <- t;
         done
    let opt_best ?(debug = false) ?(maxi = -1) eval path =
        let bound = Array.length path in

        let rec loop k =
            let diff = ref 0 in
            let minI, minJ = ref 0, ref 0 in
                for i = 0 to bound - 4 do
                    for j = i+2 to bound - 1 - max 0 (1-i) do
                        let d = eval path.(i) path.(j) + eval path.(i+1) path.((j+1) mod bound)
                        - eval path.(i) path.(i+1) - eval path.(j) path.((j+1) mod bound)
                        in
                        if d < !diff then (
                            diff := d;
                            minI := i;
                            minJ := j
                        )
                    done
                done;
            if !diff < 0 then (
                invertPath !minI !minJ path;
                if debug then Printf.printf "\ninverted %d and %d" !minI !minJ;
                if k < maxi || maxi < 0 then loop (k+1)
            )
        in loop 1

    let opt_fast ?(debug = false) ?(maxi = -1) eval path =
        let bound = Array.length path in
        let rec rec_while i = (i < maxi || maxi < 0) &&
            not (loop1 0) && rec_while (i+1)
        and loop1 i = i >= bound - 4 || (loop2 i (i+2) && loop1 (i+1))
        and loop2 i j = j >= bound - max 0 (1-i)  || (
            let diff = eval path.(i) path.(j) + eval path.(i+1) path.((j+1) mod bound)
                                   - eval path.(i) path.(i+1) - eval path.(j) path.((j+1) mod bound)  in
            if diff < 0 then (
                invertPath i j path;
                if debug then Printf.printf "\ninverted %d and %d, diff : %d" i j diff;
                false
            ) else true
        ) && loop2 i (j+1)
        in
        let _ = rec_while 0 in ()

    type random_creation = Roulette | Random

    let weight_update eval last q = function
        | Random -> ()
        | Roulette -> RndQ.change_weights (fun _ x -> 1. /. float_of_int(eval x last)) q

    let random_path q eval mode city_count =
        Array.init city_count (
            fun _ -> let v = RndQ.take q in
                weight_update eval v q mode;
                v
        )

    let iter_two_opt n eval city_count rnd_mode =
        let arr = Array.init city_count (Fun.id) in
        let q = RndQ.simple_create city_count arr in
        let best_len = ref max_int in
        let best_path = Array.make city_count (-1) in
        for _ = 1 to n do
            let path = random_path q eval rnd_mode city_count in
            opt_fast eval path;
            let len = Basetsp.path_length eval path in
            if len < !best_len then (
                best_len := len;
                for i = 0 to city_count -1 do
                    best_path.(i) <- path.(i)
                done
            );
            RndQ.reset q

        done;
        best_path

end;;
module Primalg = struct
    let primalg eval city_count =
        let init_visited = IntSet.singleton 0 in
        let init_visit =
            let rec aux1 i acc =
            if i < city_count then aux1 (i + 1) (IntSet.add i acc) else acc
            in aux1 1 IntSet.empty
        in
        let rec aux to_visit visited score =
            if not @@ IntSet.is_empty to_visit then (
                let added = ref (-1) in
                let mini = ref max_int in
                IntSet.iter (fun c1 ->
                    IntSet.iter (fun c2 ->
                        let len = eval c1 c2 in
                        if len < !mini then (
                            mini := len;
                            added := c1
                        )) visited) to_visit;
                let new_to_visit = IntSet.remove !added to_visit
                in
                let new_visited = IntSet.add !added visited
                in
                aux new_to_visit new_visited (score +  !mini)
            ) else (
                score
            )
        in
        aux init_visit init_visited 0
end;;

module Monte_Carlo = struct

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
                      current_path : int array; best_path : int array; mutable best_score : int}
    (* [FR] Type contenant tous les arguments qui n'auront donc pas besoin d'�tre pass�s
    dans les diff�rentes fonctions *)
    (* [EN] Type containing all the info needed in the functions in order to avoid
    useless arguments*)

    let arg = ref {playout_selection_mode=Random; visited = IntSet.empty; city_count= -1; path_size = -1;
                   eval = (fun _ _ -> -1); get_node_score = (fun _ -> -1.); current_path = [||]; best_path = [||];
                   best_score = -1}
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
        let cities = try (Array.init size (fun _ -> aux())) with Invalid_argument e -> raise @@
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
        let end_path =
        try (Array.init size (fun _ -> let c = RndQ.take queue in update_weights queue c; c)) with Invalid_argument _ ->
        failwith "end_path"
        in
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
                best_path = Array.make city_count (-1); best_score = max_int};
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
        Printf.printf "%d playouts, %.0f s, %d length" !playout_count (Sys.time() -. start_time) !arg.best_score;
        !arg.best_path


end;;module Simulated_Annealing = struct

    type random_change_mode = Swap | Insert | Invert

    type argument = {mutable problem_size : float; mutable city_count : int; mutable solution : int array; mutable eval : int -> int -> int;
        mutable i : int; mutable j : int}

    let arg = {problem_size = -1.; solution = [||]; eval = (fun _ _ -> -1); i = 0; j = 1; city_count = -1}

    let random_choice() =
        let rnd shift = int_of_float @@ Random.float arg.problem_size +. 0.5 -. shift in
        let i = rnd 0. in
        let j = rnd 1. in
        let j = if j = i then int_of_float @@ arg.problem_size +. 0.5 else j in
        arg.i <- i;
        arg.j <- j

    let get_delta_fun mode =
        let m = function
            | -1 -> arg.city_count - 1
            | x when x = arg.city_count -> 0
            | x -> x
        in
        let edge_len x y = arg.eval arg.solution.(x) arg.solution.(y)
        in
        fun () -> let i, j = arg.i, arg.j in
        match mode with
            | Swap -> edge_len (m @@ i-1) j + edge_len (m @@ i+1) j + edge_len (m @@ j-1) i + edge_len (m @@ j+1) i
                    - edge_len (m @@ i-1) i - edge_len (m @@ i+1) i - edge_len (m @@ j-1) j - edge_len (m @@ j+1) j
            | _ -> failwith "not implemented yet"

    let get_randomize_fun mode =
        fun () ->
        match mode with
            | Swap -> let m = arg.solution.(arg.i) in
                        arg.solution.(arg.i) <- arg.solution.(arg.j);
                        arg.solution.(arg.j) <- m;
            | _ -> failwith "not implemented yet"


    let start_sa city_count eval init_T final_T iter_count cooling_ratio rnd_mode =
        let q = RndQ.simple_create city_count @@ Array.init city_count Fun.id in
        arg.solution <- RndQ.tot_empty q;
        arg.problem_size <- float_of_int @@ city_count - 1;
        arg.eval <- eval;
        arg.city_count <- city_count;
        let randomize = get_randomize_fun rnd_mode in
        let delta = get_delta_fun rnd_mode in
        let rec aux temp =
            if temp <= final_T then arg.solution else
            begin
                for _ = 1 to iter_count do
                    random_choice();
                    if Random.float 1. < exp (-. float_of_int (delta()) /. temp) then randomize()
                done;
                aux @@ temp *. cooling_ratio
            end
        in aux init_T



end;;