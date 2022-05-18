let city_config = "TSPLIB/dsj1000"


let city_count, cities = Reader_tsp.open_tsp city_config

let adj_matrix = Base_tsp.get_adj_matrix cities

let () =
  Printexc.record_backtrace true;
  let start_time = Sys.time () in
  let result = Prim_Alg.prim_alg (fun i j -> adj_matrix.(i).(j)) city_count in
  Printf.printf "MST length : %d in %fs" result (Sys.time () -. start_time)
