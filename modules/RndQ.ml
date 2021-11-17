
(*  permettant de cr�er une file al�atoire : chaque �l�ment
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
