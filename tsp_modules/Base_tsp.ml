let get_adj_matrix cities = 
    let dist (c1x,c1y) (c2x,c2y) =
        int_of_float (0.5 +. sqrt ((c1x -. c2x)*.(c1x -. c2x) +. (c1y -. c2y)*. (c1y -. c2y)))
    in
    let city_count = Array.length cities in
    Array.init city_count (fun i -> Array.init city_count (fun j -> dist cities.(i) cities.(j)))
    
let dists cities =
    let adj_matrix = get_adj_matrix cities in 
    fun c1 c2 -> try (adj_matrix.(c1).(c2)) with Invalid_argument e -> raise @@ 
        Invalid_argument (Printf.sprintf "get dist %d %d failed : " c1 c2 ^ e)


let path_length eval path =
    let s = ref 0 in
    for i = 0 to Array.length path - 2 do
        s := !s + eval path.(i) path.(i+1)
    done;
    !s + eval path.(0) path.(Array.length path - 1)

let random_path city_count = 
    let rndQ = Random_Queue.simple_create city_count (Array.init city_count Fun.id) in 
    Random_Queue.tot_empty rndQ

let best_path_length config eval =
    let path = Reader_tsp.open_path config in
    path_length eval path

let print_path ?(oc = stdout) path = 
    Array.iteri (fun i v -> (if i = 0 then Printf.fprintf oc "[%d] " else 
        if i = Array.length path - 1 then Printf.fprintf oc "-> [%d]\n" 
        else Printf.fprintf oc "-> %d ") v) path

let prerr_path = 
    print_path ~oc:stderr 
let print_best_path ?(oc=stdout) config = 
    Printf.fprintf oc "\nbest path for %s :\n" config;
    print_path ~oc @@ Reader_tsp.open_path config

let check_path_validity path =
    let l = Array.length path - 1 in 
    let rec search i el = i <= l && (path.(i) = el || search (i+1) el) in 
    let rec aux i = 
        i > l || (path.(i) <= l && not @@ search (i+1) path.(i) && aux (i+1)) 
    in aux 0

(* let check_path_validity *)

let path_of_string path_string = 
    (*[0] -> 2 -> 1 -> 3 -> 7 -> 15 -> 12 -> 11 -> 9 -> 8 -> 10 -> 4 -> 14 -> 5 -> 6 -> [13]*)
    let cleaned_string = Str.(global_replace (regexp "[][> \n]") "" path_string) in 
    Array.of_list @@ List.map int_of_string @@ String.split_on_char '-' cleaned_string

let print_error_ratio ?(oc=stdout) path eval city_config = 
    let len = path_length eval path in
    let best_len = best_path_length city_config eval in
    Printf.fprintf oc "\n%% of error : %.2f %%\n\n" (100. *.float_of_int(len - best_len) /. float_of_int best_len);
    print_best_path ~oc city_config
