
let city_config = "att48";;
let city_count, cities = Readertsp.open_tsp city_config;;
let eval = Basetsp.dists cities;;
let path  = MCTS2.mcts city_count eval MCTS2.Roulette MCTS2.No_opt
(MCTS2.Ucb1_ecart_type (2. *. 2. ** 0.5)) MCTS2.Arithmetic 2000000 3600.;;
Showtsp.show_solution cities path;;

let len = Basetsp.path_length eval path in
let best_len = Basetsp.best_path_length city_config eval in
    Printf.printf "\n%% of error : %.2f %%" (100. *.(len -. best_len) /. best_len);;

Readertsp.open_path city_config;;