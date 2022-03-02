open Graphics

type parameters = {
  mutable height : int;
  mutable width : int;
  mutable city_size : int;
}

let params = { height = 600; width = 600; city_size = 10 }

let coordToScreen (maxX, maxY) (x, y) =
  let a, b =
    ( (float_of_int params.width *. 0.1)
      +. (0.8 *. float_of_int params.width *. x /. maxX),
      (float_of_int params.height *. 0.1)
      +. (0.8 *. float_of_int params.height *. y /. maxY) )
  in
  (int_of_float a, int_of_float b)

let show_cities cities =
  open_graph @@ Printf.sprintf "%dx%d" params.width params.height;
  clear_graph ();
  set_color @@ rgb 40 40 40;
  fill_rect 0 0 params.width params.height;

  let fl = float_of_int in
  let city_count = Array.length cities in
  set_line_width 1;
  let update_color i =
    let r =
      int_of_float
      @@ ((fl @@ (city_count - i - 1)) /. fl (city_count - 1) *. 255.)
    in
    let b = int_of_float @@ (fl i /. fl (city_count - 1) *. 255.) in
    set_color @@ rgb r 0 b
  in
  let maxX, maxY =
    Array.fold_left
      (fun (maxX, maxY) (x, y) -> (max maxX x, max maxY y))
      (0., 0.) cities
  in
  Array.iteri (fun i (x, y) ->
      update_color i;
      fill_circle x y params.city_size)
  @@ Array.map (coordToScreen (maxX, maxY)) cities

let show_cities_names cities =
  set_text_size 60;
  set_color @@ rgb 13 165 36;
  set_color white;
  let maxX, maxY =
    Array.fold_left
      (fun (maxX, maxY) (x, y) -> (max maxX x, max maxY y))
      (0., 0.) cities
  in
  Array.iteri (fun i (x, y) ->
      let xs, ys = text_size @@ string_of_int i in
      moveto
        (x - (params.city_size * 0) - (xs / 2))
        (y - (params.city_size / 2 * 0) - (ys / 2));
      draw_string @@ string_of_int i)
  @@ Array.map (coordToScreen (maxX, maxY)) cities

let show_solution ?(title = "Tour") cities sol =
  let sorted_cities =
    Array.init (Array.length sol) (fun i -> cities.(sol.(i)))
  in

  let maxX, maxY =
    Array.fold_left
      (fun (maxX, maxY) (x, y) -> (max maxX x, max maxY y))
      (0., 0.) cities
  in
  let movetoT (x, y) = moveto x y in
  let coord = coordToScreen (maxX, maxY) in
  let lineto_city city =
    let x, y = coord cities.(city) in
    lineto x y
  in
  show_cities sorted_cities;
  set_window_title title;
  set_line_width 3;
  set_color black;
  movetoT @@ coord cities.(sol.(0));
  for k = 1 to Array.length sol - 1 do
    lineto_city sol.(k)
  done;
  lineto_city sol.(0);
  show_cities_names cities

let show_solution_and_wait ?(title = "Tour") cities sol =
  show_solution ~title cities sol;
  print_endline "enter anything to close the graphic window";
  ignore @@ input_line stdin

let show_solution_list cities sol =
  let maxX, maxY =
    Array.fold_left
      (fun (maxX, maxY) (x, y) -> (max maxX x, max maxY y))
      (0., 0.) cities
  in
  let movetoT (x, y) = moveto x y in
  let coord = coordToScreen (maxX, maxY) in
  let lineto_city city =
    let x, y = coord cities.(city) in
    lineto x y
  in
  show_cities cities;
  set_line_width 3;
  set_color black;
  match sol with
  | [] -> prerr_endline "warning : tried to display empty solution"
  | x :: xs ->
      movetoT @@ coord cities.(x);
      List.iter lineto_city xs;
      lineto_city x

let show_best_path config =
  let _, cities = Reader_tsp.open_tsp config in
  show_solution cities (Reader_tsp.open_path config)

let show_best_path_and_wait config =
  let _, cities = Reader_tsp.open_tsp config in
  show_solution_and_wait
    ~title:(Printf.sprintf "best tour for %s" config)
    cities
  @@ Reader_tsp.open_path config
