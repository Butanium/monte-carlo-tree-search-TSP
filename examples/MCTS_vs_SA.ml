let foi = float_of_int
let city_config = "att48"
let city_count, cities = Readertsp.open_tsp city_config
let eval = Basetsp.dists cities

let () = 
    let start_time = Sys.time() in
    let sa_path = Simulated_Annealing.start_sa city_count eval 10000. 0.1 1000 0.9 Simulated_Annealing.Swap in
    let max_time = Sys.time() -. start_time in
    Printf.printf "\nmax time : %.2f" max_time;
    let mcts_path  = Monte_Carlo.procede_mcts Monte_Carlo.Roulette Monte_Carlo.Min_spanning_tree city_count
        eval max_time (max_int-2) in
    let mst_length = foi @@ Primalg.primalg eval city_count in
    Printf.printf "mst ratios : \n   - mcts : %.1f\n   - sa : %.1f\n\n" ((foi @@ Basetsp.path_length eval mcts_path) /. mst_length)
        ((foi @@ Basetsp.path_length eval sa_path) /. mst_length);
    print_endline "simulated annealing path : ";
    Basetsp.print_path sa_path;
    print_endline "monte carlo path : ";
    Basetsp.print_path mcts_path;
    let title = "simulated annealing path" in 
    Showtsp.show_solution ~title cities sa_path;
    print_endline "\nsend anything to see the mcts path";
    let _ = read_line() in 
    let title = "\nmcts path" in 
    Showtsp.show_solution_and_wait ~title cities mcts_path;

