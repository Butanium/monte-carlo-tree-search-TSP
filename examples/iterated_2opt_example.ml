(*run command : dune exec ./examples/iterated_2opt_example.exe *)

let city_config = "a280"
let city_count, cities = Reader_tsp.open_tsp city_config
let eval = Base_tsp.dists cities

let max_time = 30.
let max_try = 10
let path = Two_Opt.iter_two_opt eval city_count Two_Opt.Random max_time max_try


let () = Show_tsp.show_solution_and_wait cities path;
    Base_tsp.print_error_ratio path eval city_config


