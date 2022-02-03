module RndQ = Random_Queue

let invertPath i j path =
  for k = 0 to ((j - i) / 2) - 1 do
    let t = path.(i + 1 + k) in
    path.(i + 1 + k) <- path.(j - k);
    path.(j - k) <- t
  done

let opt_best ?(debug = false) ?(partial_path = false) ?(max_iter = -1) eval path
    =
  let bound = Array.length path in
  let partial = if partial_path then 1 else 0 in
  let rec loop k =
    let diff = ref 0 in
    let minI, minJ = (ref 0, ref 0) in
    for i = 0 to bound - 4 - partial do
      for j = i + 2 to bound - 1 - max (2 * partial) (1 - i) do
        let d =
          eval path.(i) path.(j)
          + eval path.(i + 1) path.((j + 1) mod bound)
          - eval path.(i) path.(i + 1)
          - eval path.(j) path.((j + 1) mod bound)
        in
        if d < !diff then (
          diff := d;
          minI := i;
          minJ := j)
      done
    done;
    if !diff < 0 then (
      invertPath !minI !minJ path;
      if debug then Printf.printf "\ninverted %d and %d" !minI !minJ;
      if k < max_iter || max_iter < 0 then loop (k + 1))
  in
  loop 1

exception Timed_Out

let opt_fast ?(debug = false) ?(partial_path = false) ?(max_iter = -1)
    ?(max_time = infinity) ?(lower_bound = 0) ?(upper_bound = -1) eval path =
  (* If [partial_path] is set to true the algorithm won't try to optimize the edge between the end of the path and the beginning.
     It's useful if you want to optimize the part of a path *)
  let bound =
    if upper_bound < 0 then Array.length path
    else min upper_bound @@ Array.length path
  in
  let bounded i = if i >= bound then i - bound else i in
  let partial = if partial_path then 1 else 0 in

  let start_time = if max_time = infinity then 0. else Sys.time () in
  (* SI max_time = infinity ne pas appeler Sys.time() *)
  let rec rec_while i =
    (i < max_iter || max_iter < 0)
    && (not (loop1 lower_bound))
    && rec_while (i + 1)
  and loop1 i = i >= bound - 3 - partial || (loop2 i (i + 2) && loop1 (i + 1))
  and loop2 i j =
    if max_time <> infinity && Sys.time () -. start_time > max_time then
      raise Timed_Out;
    j >= bound - max (2 * partial) (1 - i)
    || (let diff =
          eval path.(i) path.(j)
          + eval path.(i + 1) path.(bounded (j + 1))
          - eval path.(i) path.(i + 1)
          - eval path.(j) path.(bounded (j + 1))
        in
        if diff < 0 then (
          invertPath i j path;
          if debug then Printf.printf "\ninverted %d and %d, diff : %d" i j diff;
          false)
        else true)
       && loop2 i (j + 1)
  in
  try
    let _ = rec_while 0 in
    ()
  with Timed_Out -> ()

type random_creation = Roulette | Random

let string_of_random_mode = function
  | Random -> "Random"
  | Roulette -> "Roulette"

let weight_update eval last q = function
  | Random -> ()
  | Roulette ->
      RndQ.change_weights (fun _ x -> 1. /. float_of_int (eval x last)) q

let randomize_path q eval mode path_arr =
  for i = 0 to Array.length path_arr - 1 do
    let v = RndQ.take q in
    weight_update eval v q mode;
    path_arr.(i) <- v
  done

(* type debug = {mutable } *)

let iter_two_opt ?city_config ?name ?(verbose = true) ?logs_path eval city_count
    rnd_mode max_time max_try =
  
  Random.self_init ();
  let create_arr () = Array.init city_count Fun.id in
  let queue = RndQ.simple_create city_count @@ create_arr () in
  let path_arr = create_arr () in
  let best_len = ref max_int in
  let best_path = create_arr () in
  let i = ref 0 in
  let acc_scores = ref 0 in
  let start_time = Sys.time () in
  let get_time () = Sys.time () -. start_time in
  let total_randomize_time = ref 0. in
  while !i < max_try && (max_time = infinity || get_time () < max_time) do
    randomize_path queue eval rnd_mode path_arr;
    let max_time = if max_time = infinity then infinity else max_time -. get_time () in
    opt_fast ~max_time eval path_arr;
    let len = Base_tsp.path_length eval path_arr in
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
      "iterated two opt achieved in %.1f s (%f s of randomization), %d \
       iterations.\n\
       %s%s\n\
       Best score : %d | Average score : %d\n"
      (get_time ()) !total_randomize_time !i
      (match name with
      | None -> ""
      | Some s -> Printf.sprintf "Simulation %s " s)
      (match city_config with
      | None -> ""
      | Some s -> Printf.sprintf "city config : %s" s)
      !best_len (!acc_scores / !i);
    Printf.fprintf oc "best path : ";
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
