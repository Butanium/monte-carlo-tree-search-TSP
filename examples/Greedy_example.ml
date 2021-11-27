(*run command : dune exec ./examples/Greedy_example.exe *)

let city_config = "att48"

let city_count, cities = Reader_tsp.open_tsp city_config
let eval = Base_tsp.dists cities
let max_time = 1800.
let max_try = max_int

let random_mode = Greedy_Random.Roulette
let path = Greedy_Random.greedy eval city_count random_mode max_time max_try


let () = Show_tsp.show_solution_and_wait cities path;
    let len = Base_tsp.path_length eval path in
    let best_len = Base_tsp.best_path_length city_config eval in
    Printf.printf "\n%% of error : %.2f %%" (100. *.float_of_int(len - best_len) /. float_of_int best_len);  
    Base_tsp.print_best_path city_config

