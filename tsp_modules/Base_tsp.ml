
let dists cities =
    let dist (c1x,c1y) (c2x,c2y) =
        int_of_float (0.5 +. sqrt ((c1x -. c2x)*.(c1x -. c2x) +. (c1y -. c2y)*. (c1y -. c2y)))
    in
    let city_count = Array.length cities in
    let adj_matrix = Array.init city_count (fun i -> Array.init city_count (fun j -> dist cities.(i) cities.(j)))
    in
    fun c1 c2 -> try (adj_matrix.(c1).(c2)) with Invalid_argument e -> raise @@ 
        Invalid_argument (Printf.sprintf "get dist %d %d failed : " c1 c2 ^ e)

let path_length eval path =
    let s = ref 0 in
    for i = 0 to Array.length path - 2 do
        s := !s + eval path.(i) path.(i+1)
    done;
    !s + eval path.(0) path.(Array.length path - 1)

let best_path_length config eval =
    let path = Reader_tsp.open_path config in
    path_length eval path

let print_path path = 
    Array.iteri (fun i v -> (if i = 0 then Printf.printf "[%d] " else 
        if i = Array.length path - 1 then Printf.printf "-> [%d]\n" 
        else Printf.printf "-> %d ") v) path

let prerr_path path = 
    let prerrf = fun f x-> prerr_string (Printf.sprintf f x) in 
    Array.iteri (fun i v -> (if i = 0 then prerrf "[%d] " else 
        if i = Array.length path - 1 then prerrf "-> [%d]\n" 
        else prerrf "-> %d ") v) path
let print_best_path config = 
    Printf.printf "\nbest path for %s :\n" config;
    print_path @@ Reader_tsp.open_path config

let check_path_validity path =
    let l = Array.length path - 1 in 
    let rec search i el = i <= l && (path.(i) = el || search (i+1) el) in 
    let rec aux i = 
        i > l || (path.(i) <= l && not @@ search (i+1) path.(i) && aux (i+1)) 
    in aux 0

let path_of_string path_string = 
    (*[0] -> 2 -> 1 -> 3 -> 7 -> 15 -> 12 -> 11 -> 9 -> 8 -> 10 -> 4 -> 14 -> 5 -> 6 -> [13]*)
    let cleaned_string = Str.(global_replace (regexp {|[][> \n]|}) "" path_string) in 
    Array.of_list @@ List.map int_of_string @@ String.split_on_char '-' cleaned_string
    

