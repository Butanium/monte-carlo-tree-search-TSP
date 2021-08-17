let city_count, cities = Readertsp.open_tsp "att48";;
let eval = Basetsp.dists cities;;
let s = MCTS.mcts city_count eval MCTS.Roulette MCTS.No_opt (MCTS.Ucb1_mst (2. ** 0.5)) 
    MCTS.Arithmetic  1.1 100 20000 20.;;
Showtsp.show_solution_list cities s;;
let len = let len = Basetsp.path_length eval path in
let best_len = Basetsp.best_path_length city_config eval in
    Printf.printf "\n%% of error : %.2f %%" (100. *.float_of_int(len - best_len) /. float_of_int best_len);;





