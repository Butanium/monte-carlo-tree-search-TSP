module RndQ = Random_Queue


type random_creation = Roulette | Random

let string_of_rnd_mode = function | Random -> "Random" | Roulette -> "Roulette"

let weight_update eval last q = function
    | Random -> ()
    | Roulette -> RndQ.change_weights (fun _ x -> 1. /. float_of_int(eval x last)) q

let greedy ?(generate_log_file = true) eval city_count rnd_mode max_time max_try =
  let best_scores_hist = ref [] in
  let scores_hist = ref [] in 
  
  let best_path = Array.init city_count Fun.id in 
  let get_path_length = Base_tsp.path_length eval in 
  let best_score = ref @@ get_path_length best_path in
  let path = Array.copy best_path in 
  let queue = RndQ.simple_create city_count @@ Array.sub path 1 (city_count-1)  in
  let try_count = ref 0 in 
  let start_time = Sys.time() in
  let get_time () = Sys.time() -. start_time in 
  while get_time () < max_time && !try_count < max_try do 
    RndQ.reset queue;
    incr try_count;
    for i = 1 to city_count - 1 do
      weight_update eval (path.(i-1)) queue rnd_mode;
      path.(i) <- RndQ.take queue;
    done;
    let length = get_path_length path in 
    if length < !best_score then (
      best_score := length;
      for i = 1 to city_count - 1 do 
        best_path.(i) <- path.(i)
      done;
      best_scores_hist := (get_time(), !try_count, length) :: !best_scores_hist
    );
    scores_hist := (get_time(), length) :: !scores_hist;
  done;
  Printf.printf "completed in %.0f seconds with %d tries, %d best score found\n" (get_time()) !try_count !best_score;
  if generate_log_file then begin 
    let suffix = Printf.sprintf "-Greedy_%s-%.0f_s-%d_tries" 
      (string_of_rnd_mode rnd_mode) (get_time()) !try_count in 
    let file_path, file_name = "logs/score_logs", "all_scores" ^ suffix in 
    let file = File_log.create_file ~file_path ~file_name () in 
    let _ = File_log.log_datas (fun (t, s) -> Printf.sprintf "%g,%d;" t s) file @@ List.rev !scores_hist in
    let file_path, file_name = "logs/best_score_logs", "best_scores" ^ suffix in 
    let file = File_log.create_file ~file_path ~file_name () in
    let _ = File_log.log_datas (fun (t,x,y) -> Printf.sprintf "%d,%g,%d;" x t y) file  
      @@ List.rev @@ (get_time(),!try_count, !best_score) :: !best_scores_hist in
    let start = String.length "best_scores" + 1 in
    Printf.printf "simulation ref for log files : %s\n" @@ 
        String.sub file.file_name start @@
            String.length file.file_name - start;
  end;
  best_path
  
