let city_count, cities = Readertsp.open_tsp "att48";;
let eval = Basetsp.dists cities;;
let s = MCTS.mcts city_count eval MCTS.Random MCTS.No_opt (MCTS.Ucb1_mst (2. ** 0.5))  1.1 100 10000 10.;;
Showtsp.show_solution_list cities s;;




