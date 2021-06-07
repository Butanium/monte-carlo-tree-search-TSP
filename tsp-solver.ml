let city_count = 50;; (* the amount of city *)
#load "graphics.cma";;
open Graphics;;
type city = {x:float; y:float};;
let ic = open_in "C:/Users/Clement/Documents/prépa/algogen/saves/save_2.txt";;
let cities = Array.init city_count
    (fun _ ->
        let x::y::[] = List.map (fun s -> float_of_string @@ String.trim s)
        @@ let s = input_line ic in String.split_on_char ',' @@ String.sub s 1 (String.length s - 2)
            in {x;y}
    );;

let dist c1 c2 = ((c1.x -. c2.x)**2. +. (c1.y -. c2.y)**2.)**0.5;;



let adj_matrix = Array.init city_count (fun i -> Array.init city_count (fun j -> dist cities.(i) cities.(j)));;
let eval i j = adj_matrix.(i).(j);;
let width = 600;;
let height = 600;;
let f = float_of_int;;
let fint = int_of_float;;
let coordToScreen (maxX, maxY) {x;y} =
       let a,b = f width *. 0.1 +. 0.8 *. f width *. x /. maxX,
        f height *. 0.1 +. 0.8 *. f height *. y /. maxY
    in fint a, fint b;;
let show_cities cities = open_graph @@ Printf.sprintf "%dx%d" width height;
    clear_graph();

    set_line_width 1;
    let r = 10 in
    let maxX, maxY = Array.fold_left (fun (maxX, maxY) {x;y} -> (max maxX x), (max maxY y)) (0.,0.) cities in

    Array.iteri (fun i (x,y) ->  set_color red; fill_circle x y r;set_text_size 10;set_color black; moveto x y; draw_string @@ string_of_int i) @@ Array.map (coordToScreen (maxX, maxY)) cities;;


let print_solution cities sol =
    let maxX, maxY = Array.fold_left (fun (maxX, maxY) {x;y} -> (max maxX x), (max maxY y)) (0.,0.) cities
    in
    let movetoT (x,y)= moveto x y
    in
    let coord = coordToScreen (maxX, maxY)
    in
    let lineto_city city = let x,y = coord cities.(city) in lineto x y
    in
    show_cities cities;
    set_line_width 3;
    set_color black;
    movetoT @@ coord cities.(sol.(0)) ;
    for k = 1 to Array.length sol - 1 do
        lineto_city sol.(k);

    done;
    lineto_city sol.(0);;
let id =  Array.init (Array.length cities) Fun.id;;
    print_solution cities id ;;
let s = Array.init (Array.length cities) Fun.id;;
TwoOpt.optBest (TwoOpt.make_arg city_count eval) (-1) s;;
print_solution cities s ;;
