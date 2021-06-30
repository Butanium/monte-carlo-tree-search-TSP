module Showtsp = struct
    open Graphics
    type parameters = {mutable height: int; mutable width: int; mutable city_size: int}

    let params = {height=600; width=600; city_size=10}

    let coordToScreen (maxX, maxY) (x,y) =
           let a,b = float_of_int params.width *. 0.1 +. 0.8 *. float_of_int params.width *. x /. maxX,
            float_of_int params.height *. 0.1 +. 0.8 *. float_of_int params.height *. y /. maxY
        in int_of_float a, int_of_float b

    let show_cities cities =
        open_graph @@ Printf.sprintf "%dx%d" params.width params.height;
        clear_graph();
        set_line_width 1;
        let maxX, maxY = Array.fold_left (fun (maxX, maxY) (x,y) -> (max maxX x), (max maxY y)) (0.,0.) cities in
        Array.iteri (fun i (x,y) ->  set_color red; fill_circle x y params.city_size;set_text_size 20;set_color black;
                    moveto x y; draw_string @@ string_of_int i) @@ Array.map (coordToScreen (maxX, maxY)) cities

    let show_solution cities sol =
        let maxX, maxY = Array.fold_left (fun (maxX, maxY) (x,y) -> (max maxX x), (max maxY y)) (0.,0.) cities
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
        lineto_city sol.(0)
        
    let show_solution_list cities sol =
        let maxX, maxY = Array.fold_left (fun (maxX, maxY) (x,y) -> (max maxX x), (max maxY y)) (0.,0.) cities
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
        let x :: xs = sol in 
            
        movetoT @@ coord cities.(x);
        List.iter lineto_city xs;
        lineto_city x
    let show_best_path config =
        let _, cities = Readertsp.open_tsp config in
        show_solution cities (Readertsp.open_path config)
    
end;;
