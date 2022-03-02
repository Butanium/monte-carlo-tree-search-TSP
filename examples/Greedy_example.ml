(*run command : dune exec ./examples/Greedy_example.exe *)

let city_config = "att48"

let city_count, cities = Reader_tsp.open_tsp city_config

let adj_matrix = Base_tsp.get_adj_matrix cities

let max_time = 1800.

let max_try = max_int

let random_mode = Greedy_Random.Roulette

let tour =
  Greedy_Random.greedy adj_matrix city_count random_mode max_time max_try

let () =
  Show_tsp.show_solution_and_wait cities tour;
  Base_tsp.print_error_ratio tour adj_matrix city_config
