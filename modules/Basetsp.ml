module Basetsp = struct
    let dists cities =
        let dist (c1x,c1y) (c2x,c2y) = ((c1x -. c2x)**2. +. (c1y -. c2y)**2.)**0.5
        in
        let city_count = Array.length cities in
        let adj_matrix = Array.init city_count (fun i -> Array.init city_count (fun j -> dist cities.(i) cities.(j)))
        in
        fun c1 c2 -> adj_matrix.(c1).(c2)
end;;