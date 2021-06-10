let city_count, cities = Readertsp.open_path "ulysses16" in
let eval = Basetsp.dists cities in
let s = MCTS.mcts city_count eval MCTS.Random (Ucb1 2. ** 0.5) MCTS.Random  0.9 1000 10. in s;;