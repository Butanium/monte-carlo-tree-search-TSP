(*run command : dune exec ./examples/MCTS_vs_SA.exe *)

let foi = float_of_int
let city_config = "att48"
let city_count, cities = Reader_tsp.open_tsp city_config
let eval = Base_tsp.dists cities

let () = 
    let start_time = Sys.time() in
    let sa_path = Simulated_Annealing.start_sa city_count eval 10000. 0.1 1000 0.9 Simulated_Annealing.Swap in
    let max_time = Sys.time() -. start_time in
    Printf.printf "\nmax time for mcts : %.4f seconds" max_time;
    let (mcts_path,_), _, _  = MCTS.proceed_mcts ~debug_tree:false ~generate_log_file:false city_count
        eval max_time (max_int-2) in
    let mst_length = foi @@ Prim_Alg.prim_alg eval city_count in
    Printf.printf "mst ratios : \n   - mcts : %.2f\n   - sa : %.2f\n\n" ((foi @@ Base_tsp.path_length eval mcts_path) /. mst_length)
        ((foi @@ Base_tsp.path_length eval sa_path) /. mst_length);
    print_endline "simulated annealing path : ";
    Base_tsp.print_path sa_path;
    print_endline "monte carlo path : ";
    Base_tsp.print_path mcts_path;
    let title = "simulated annealing path" in 
    Show_tsp.show_solution ~title cities sa_path;
    print_endline "\nsend anything to see the mcts path";
    let _ = read_line() in 
    let title = "\nmcts path" in 
    Show_tsp.show_solution_and_wait ~title cities mcts_path;

