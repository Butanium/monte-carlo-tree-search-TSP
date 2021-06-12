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
            Printf.sprintf "C:/Users/Clement/Documents/prépa/tipe/ocaml-tsp/tsp/%s.tsp" tsp_name
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
            Printf.sprintf "C:/Users/Clement/Documents/prépa/tipe/ocaml-tsp/tsp/%s.opt.tour" tsp_name
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
