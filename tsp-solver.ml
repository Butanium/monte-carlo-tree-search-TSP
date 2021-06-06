let city_count = 50;; (* the amount of city *)
type city = {x:float; y:float};;
let ic = open_in "C:/Users/Clement/Documents/prépa/algogen/saves/save_1.txt";;
let pos = Array.init city_count
    (fun _ ->
        let x::y::[] = List.map (fun s -> float_of_string @@ String.trim s)
        @@ let s = input_line ic in String.split_on_char ',' @@ String.sub s 1 (String.length s - 2)
            in {x;y}
    );;
let dist c1 c2 = ((c1.x -. c2.x)**2. +. (c1.y -. c2.y)**2.)**0.5;;

let adj_matrix = Array.init city_count (fun i -> Array.init city_count (fun j -> dist pos.(i) pos.(j)));;

