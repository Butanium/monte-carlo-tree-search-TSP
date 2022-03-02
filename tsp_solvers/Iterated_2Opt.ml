open Two_Opt

type random_creation = Roulette | Random

let string_of_random_mode = function
  | Random -> "Random"
  | Roulette -> "Roulette"

let weight_update adj last q = function
  | Random -> ()
  | Roulette -> RndQ.roulette_weights adj last q

let randomize_path q adj mode path_arr =
  for i = 0 to Array.length path_arr - 1 do
    let v = RndQ.take q in
    weight_update adj v q mode;
    path_arr.(i) <- v
  done

(* type debug = {mutable } *)

let iter_two_opt ?city_config ?name ?(verbose = true) ?logs_path ?seed
    ?(check_time = 10) adj_matrix city_count rnd_mode max_time max_try =
  let seed =
    match seed with
    | None ->
        Random.self_init ();
        Random.int 1073741823
    | Some s -> s
  in
  Random.init seed;
  let create_arr () = Array.init city_count Fun.id in
  let queue = RndQ.simple_create city_count @@ create_arr () in
  let path_arr = create_arr () in
  let best_len = ref max_int in
  let best_path = create_arr () in
  let i = ref 0 in
  let acc_scores = ref 0 in
  let start_time = Unix.gettimeofday () in
  let get_time () = Unix.gettimeofday () -. start_time in
  let last_time_check = ref 0 in
  while
    !i < max_try
    && (max_time = infinity
       || (incr last_time_check;
           if !last_time_check > check_time then (
             last_time_check := 0;
             false)
           else true)
       || get_time () < max_time)
  do
    randomize_path queue adj_matrix rnd_mode path_arr;
    let max_time =
      if max_time = infinity then infinity else max_time -. get_time ()
    in
    ignore (opt_fast ~max_time adj_matrix path_arr);
    let len = Base_tsp.path_length adj_matrix path_arr in
    if len < !best_len then (
      best_len := len;
      for i = 0 to city_count - 1 do
        best_path.(i) <- path_arr.(i)
      done);
    incr i;
    acc_scores := !acc_scores + len;
    RndQ.reset queue
  done;
  let debug oc =
    Printf.fprintf oc
      "iterated two opt achieved in %.1f s, %d iterations.\n\
       %s%s\n\
       Best score : %d | Average score : %d\n"
      (get_time ()) !i
      (match name with
      | None -> ""
      | Some s -> Printf.sprintf "Simulation %s " s)
      (match city_config with
      | None -> Printf.sprintf "seed : %d" seed
      | Some s -> Printf.sprintf "city config : %s, seed : %d" s seed)
      !best_len (!acc_scores / !i);
    Printf.fprintf oc "best tour : ";
    Base_tsp.print_path ~oc best_path
  in
  if verbose then debug stdout;
  (match logs_path with
  | None -> ()
  | Some log_path ->
      let file_path =
        File_log.create_log_dir
        @@ Printf.sprintf "%s/%s" log_path
             (match name with
             | None ->
                 Printf.sprintf "%sIterated2Opt"
                   (match city_config with None -> "" | Some c -> c ^ "-")
             | Some name -> name)
      in
      let file =
        File_log.create_file ~file_path ~file_name:"output" ~extension:"txt" ()
      in
      let oc = File_log.get_oc file in
      debug oc;
      close_out oc);
  best_path
