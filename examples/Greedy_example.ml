(*run command : dune exec ./examples/Greedy_example.exe *)

let city_config = "att48"

let city_count, cities = Reader_tsp.open_tsp city_config
let eval = Base_tsp.dists cities
let max_time = 1800.
let max_try = max_int

let random_mode = Greedy_Random.Roulette
let path = Greedy_Random.greedy eval city_count random_mode max_time max_try


let () = Show_tsp.show_solution_and_wait cities path;
    Base_tsp.print_error_ratio path eval city_config


