(* Permet de créer une file aléatoire : chaque élément
   a un certain poids qui détermine la probabilité qu'il a d'être tiré *)

exception Empty

type 'a t = {
  mutable size : int;
  content : ('a * float) array;
  mutable tot : float;
  mutable is_simple : bool;
}

(* [size], la taille actuelle de la file, [content] une array dont les [size] premiers éléments
   représentent les éléments restant dans la file avec leur probabilité, [tot] est la somme des probabilités *)
let replace_element queue index element =
  let _, w = queue.content.(index) in
  queue.content.(index) <- (element, w)
(* Remplace l'élément à l'index [index] de la queue par [element]*)

let simple_create size arr =
  {
    size;
    content = Array.map (fun x -> (x, 1.)) arr;
    tot = float_of_int size;
    is_simple = true;
  }
(* Créer une file aléatoire contenant les [size] premiers éléments de [arr], oé tous les éléments
   ont les mêmes chances de sortir *)

let create size arr weights =
  let tot = Array.fold_left ( +. ) 0. weights in
  {
    size;
    content = Array.mapi (fun i x -> (x, weights.(i))) arr;
    tot;
    is_simple = false;
  }
(* Créer une file aléatoire contenant les [size] premiers éléments de [arr] oé les chances de sortir
   d'un élément de arr est pondéré par l'élément de même index de [weights] *)

let is_empty q = q.size = 0
(* Renvoie true si la file est vide *)

let get_length q = q.size
(* Renvoie la taille de la file *)

let set_size q size =
  if size > Array.length q.content then
    raise @@ Invalid_argument "Given size is bigger than content size";
  q.size <- size;
  q.tot <-
    (let acc = ref 0. in
     for i = 0 to size - 1 do
       let _, w = q.content.(i) in
       acc := !acc +. w
     done;
     !acc)

let take q =
  (* Sélectionne aléatoirement un élément *)
  if q.size = 0 then raise Empty
  else
    let rec aux k acc =
      if k >= q.size - 1 then q.size - 1
      else
        let acc =
          acc
          -.
          let _, p = q.content.(k) in
          p
        in
        if acc < 1e-10 then k else aux (k + 1) acc
    in
    let i =
      if q.is_simple then Random.int q.size
      else aux 0 @@ (Random.float 1. *. q.tot)
    in
    (* [i] l'index sélectionné aléatoirement, si la queue est simple c'est en O(1) sinon en O(n).
       On pourrait peut être réduire le temps moyen d'exécution en triant par ordre décroissant de poids les éléments *)
    let ((res, p) as r) = q.content.(i) in
    q.content.(i) <- q.content.(q.size - 1);
    (* l'élément i sélectionné est remplacé par le dernier élément de la file *)
    q.content.(q.size - 1) <- r;
    (* On conserve l'élément sélectionné dans le tableau mais il ne fait plus partie de la file.
       On le conserve afin de pouvoir réutiliser la file si besoin *)
    q.size <- q.size - 1;
    (* taille réduite de 1 *)
    q.tot <- q.tot -. p;
    (* Poids total réduit de la probabilité de l'élément choisi *)
    res

let tot_empty q = Array.init q.size (fun _ -> take q)
(* Créer une array contenant les éléments restant dans la file, dans un ordre aléatoire. *)

let change_weights f q =
  let x0, w0 = q.content.(0) in
  let new_w0 = f w0 x0 in
  q.content.(0) <- (x0, new_w0);
  q.is_simple <- true;
  let tot = ref new_w0 in
  for i = 1 to q.size - 1 do
    let x, w = q.content.(i) in
    let new_w = f w x in
    q.content.(i) <- (x, new_w);
    tot := !tot +. new_w;
    q.is_simple <- q.is_simple && new_w = new_w0
  done;
  q.tot <- !tot
(* change les poids des différents éléments selon f *)

let roulette_weights adj_matrix last q =
  let x0, _ = q.content.(0) in
  let new_w0 = 1. /. (float @@ adj_matrix.(last).(x0)) in
  q.content.(0) <- (x0, new_w0);
  q.is_simple <- false;
  let tot = ref new_w0 in
  for i = 1 to q.size - 1 do
    let x, _ = q.content.(i) in
    let new_w = 1. /. (float @@ adj_matrix.(last).(x0)) in
    q.content.(i) <- (x, new_w);
    tot := !tot +. new_w
  done;
  q.tot <- !tot

(* spécifique a notre application, change les weights inversement décroissant par rapport à la distance
   de la ville à `last`*)
let reset q =
  (* Remet tous les éléments déjà tirés dans la file *)
  q.size <- Array.length q.content;
  q.tot <- Array.fold_left (fun acc (_, w) -> acc +. w) 0. q.content
