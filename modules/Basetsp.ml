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
