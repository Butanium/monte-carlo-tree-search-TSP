module RndQ = struct
(* Module permettant de créer une file aléatoire : chaque élément
à un certain poids qui détermine la probabilité qu'il a d'être tiré *)

    exception Empty

    type 'a t = {mutable size: int; content : ('a * float) array; mutable tot : float}
    (** [size], la taille actuelle de la file, [content] une array dont les [size] premiers éléments
    représentent les éléments restant dans la file avec leur probabilité, [tot] est la somme des probabilités*)

    let simple_create size arr =
        {size; content = Array.map (fun x -> x, 1.) arr; tot = float_of_int size}
    (* Créer une file aléatoire contenant les [size] premiers éléments de [arr], où tous les éléments
    ont les mêmes chances de sortir *)

    let create size arr weights = let tot = Array.fold_left (+.) 0. weights in
        {size; content = Array.mapi (fun i x -> x, weights.(i)) arr; tot}
    (* Créer une file aléatoire contenant les [size] premiers éléments de [arr] où les chances de sortir
    d'un élément de arr est pondéré par l'élément de même index de [weights] *)

    let is_empty q = q.size = 0
    (* Renvoie true si la file est vide *)

    let get_length q = q.size
    (* Renvoie la taille de la file *)

    let take q =
    (* Selectionne aléatoirement un élément *)
        if q.size = 0 then raise Empty else
        let rec aux k acc = if k >= q.size - 1 then k else (
            let acc = acc -. let _, p = q.content.(k) in p in
                    if acc < 1e-10 then k else aux (k+1) acc
                )
        in
        let i = aux  0 @@ Random.float 1. *. q.tot  in
        (* [i] l'index selectionné aléatoirement *)
            let res, p as r = q.content.(i) in
                q.content.(i) <- q.content.(q.size - 1);
                (* l'élément i sélectionné est remplacé par le dernier élément de la file *)
                q.content.(q.size - 1) <- r;
                (* On conserve l'élément selectionné dans l'array mais il ne fait plus partie de la file.
                On le conserve afin de pouvoir réutiliser la file si besoin *)
                q.size <- q.size - 1;
                (* taille réduite de 1 *)
                q.tot <- q.tot -. p;
                (* Poids total réduit de la probabilité de l'élément choisi *)
                res

    let tot_empty q =
        Array.init q.size (fun _ -> take q)
    (* Créer une array contenant les éléments restant dans la file, dans un ordre aléatoire. *)

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
    (* change les poids des différents éléments selon f *)

    let reset q =
    (* Remet tous les éléments déjà tirés dans la file *)
        q.size <- Array.length q.content;
        q.tot <- Array.fold_left (fun acc (_,w) -> acc +. w) 0. q.content

end;;
