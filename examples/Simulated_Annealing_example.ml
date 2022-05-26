(*run command : dune exec ./examples/Simulated_Annealing_example.exe *)
let city_config = "att48"

let city_count, cities = Reader_tsp.open_tsp city_config

let adj_matrix = Base_tsp.get_adj_matrix cities

let eval = Base_tsp.create_eval cities

let tour =
  Simulated_Annealing.start_sa city_count eval 10000. 0.1 1000 0.9
    Simulated_Annealing.Swap

let () =
  Show_tsp.show_tour_and_wait cities tour;
  Base_tsp.print_error_ratio tour adj_matrix city_config
