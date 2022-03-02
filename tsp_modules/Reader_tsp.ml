let open_tsp ?(file_path = "tsp_instances") tsp_name =
  let city_count = Scanf.sscanf tsp_name "%[^0-9]%d" (fun _ c -> c) in
  let cities = Array.make city_count (-1., -1.) in
  let fill =
    let i = ref 0 in
    fun x ->
      if !i < city_count then (
        cities.(!i) <- x;
        incr i)
  in
  let ic = open_in @@ Printf.sprintf "%s/%s.tsp" file_path tsp_name in
  let rec loop started =
    try
      let s = String.trim @@ input_line ic in
      if started then (
        let x, y = Scanf.sscanf s "%d %f %f" (fun _ x y -> (x, y)) in
        fill (x, y);
        loop true)
      else
        loop
          ("NODE_COORD_SECTION" = s
          ||
          let b = s = "DISPLAY_DATA_SECTION" in
          if b then (
            ANSITerminal.eprintf [ ANSITerminal.red ] "\n/!\\ READER WARNING :";
            Printf.eprintf
              " the tsp instance you chose is not euclidian, optimal tour was \
               not found with the euclidian distance /!\\\n\
               %!");
          b)
    with End_of_file -> ()
  in
  loop false;
  close_in ic;
  (city_count, cities)

let open_path ?(file_path = "tsp_instances") tsp_name =
  let city_count = Scanf.sscanf tsp_name "%[^0-9]%d" (fun _ c -> c) in
  let tour = Array.make city_count 0 in
  let fill =
    let i = ref 0 in
    fun x ->
      if !i < city_count then (
        tour.(!i) <- x - 1;
        incr i)
  in
  let ic = open_in @@ Printf.sprintf "%s/%s.opt.tour" file_path tsp_name in
  let rec loop started =
    try
      let s = String.trim @@ input_line ic in
      if started then (
        List.iter fill @@ List.map int_of_string @@ String.split_on_char ' ' s;
        loop true)
      else loop ("TOUR_SECTION" = s)
    with End_of_file -> ()
  in
  loop false;
  close_in ic;
  tour