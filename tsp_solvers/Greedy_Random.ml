module RndQ = Random_Queue

type random_policy = Roulette | Random

let string_of_random_policy = function
  | Random -> "Random"
  | Roulette -> "Roulette"

let weight_update eval last q = function
  | Random -> ()
  | Roulette -> RndQ.roulette_weights eval last q

let greedy ?(generate_log_file = 1) ?(logs_path = "logs") adj_matrix city_count
    rnd_policy max_time max_try =
  let best_scores_hist = ref [] in
  let scores_hist = ref [] in

  let best_tour = Array.init city_count Fun.id in
  let get_tour_length = Base_tsp.tour_length adj_matrix in
  let best_score = ref @@ get_tour_length best_tour in
  let tour = Array.copy best_tour in
  let queue =
    RndQ.simple_create city_count @@ Array.sub tour 1 (city_count - 1)
  in
  let try_count = ref 0 in
  let start_time = Unix.gettimeofday () in
  let get_time () = Unix.gettimeofday () -. start_time in
  while get_time () < max_time && !try_count < max_try do
    RndQ.reset queue;
    incr try_count;
    for i = 1 to city_count - 1 do
      weight_update adj_matrix tour.(i - 1) queue rnd_policy;
      tour.(i) <- RndQ.take queue
    done;
    let length = get_tour_length tour in
    if length < !best_score then (
      best_score := length;
      for i = 1 to city_count - 1 do
        best_tour.(i) <- tour.(i)
      done;
      best_scores_hist := (get_time (), !try_count, length) :: !best_scores_hist);
    scores_hist := (get_time (), length) :: !scores_hist
  done;
  Printf.printf "completed in %.0f seconds with %d tries, %d best score found\n"
    (get_time ()) !try_count !best_score;
  (if generate_log_file > 0 then
   let suffix =
     Printf.sprintf "-Greedy_%s-%.0f_s-%d_tries"
       (string_of_random_policy rnd_policy)
       (get_time ()) !try_count
   in
   let file_path = Printf.sprintf "%s/%s" logs_path suffix in
   let file = File_log.create_file ~file_path ~file_name:"all_scores" () in
   let _ =
     File_log.log_data (fun (t, s) -> Printf.sprintf "%g,%d;" t s) ~file
     @@ List.rev !scores_hist
   in
   let file = File_log.create_file ~file_path ~file_name:"best_scores" () in
   let _ =
     File_log.log_data (fun (t, x, y) -> Printf.sprintf "%d,%g,%d;" x t y) ~file
     @@ List.rev
     @@ ((get_time (), !try_count, !best_score) :: !best_scores_hist)
   in
   let start = String.length "best_scores" + 1 in
   Printf.printf "simulation ref for log files : %s\n"
   @@ String.sub file.file_name start
   @@ (String.length file.file_name - start));
  (!best_score, best_tour)
