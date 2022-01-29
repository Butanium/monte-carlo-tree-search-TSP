(*run command : dune exec ./examples/iterated_2opt_example.exe *)

let city_config = "a280"
let city_count, cities = Reader_tsp.open_tsp city_config
let eval = Base_tsp.dists cities

let max_time = 30.
let max_try = 100
let two_path, (mcts_path,_,_) = 
    let t = Sys.time () in
    Two_Opt.iter_two_opt eval city_count Two_Opt.Random max_time max_try, 
    MCTS.proceed_mcts ~optimization_mode:(MCTS.Two_opt {max_length=400; max_iter=400; max_time=1.}) city_count eval (Sys.time () -. t) max_int


let () = Show_tsp.show_solution_and_wait ~title:"two-opt path" cities two_path;
    Base_tsp.print_error_ratio two_path eval city_config;
    Show_tsp.show_solution_and_wait ~title:"two-opt path" cities two_path;



