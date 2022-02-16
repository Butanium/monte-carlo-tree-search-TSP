(*run command : dune exec ./examples/Simulated_Annealing_example.exe *)
let city_config = "att48"

let city_count, cities = Reader_tsp.open_tsp city_config

let eval = Base_tsp.dists cities

let path =
  Simulated_Annealing.start_sa city_count eval 10000. 0.1 1000 0.9
    Simulated_Annealing.Swap

let () =
  Show_tsp.show_solution_and_wait cities path;
  Base_tsp.print_error_ratio path eval city_config
