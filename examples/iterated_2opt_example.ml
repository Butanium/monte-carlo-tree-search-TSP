let city_config = "a280"
let city_count, cities = Reader_tsp.open_tsp city_config
let eval = Base_tsp.dists cities
let path = Two_Opt.iter_two_opt 10 eval city_count Two_Opt.Random


let () = Show_tsp.show_solution_and_wait cities path;
let len = Base_tsp.path_length eval path in
let best_len = Base_tsp.best_path_length city_config eval in
    Printf.printf "\n%% of error : %.2f %%" (100. *.float_of_int(len - best_len) /. float_of_int best_len);
    
Base_tsp.print_best_path city_config

