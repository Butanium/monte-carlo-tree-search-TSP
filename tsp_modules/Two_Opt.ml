module RndQ = Random_Queue

let max (x : int) (y : int) = if x < y then y else x

let invertPath i j path =
  for k = 0 to ((j - i) / 2) - 1 do
    let t = path.(i + 1 + k) in
    path.(i + 1 + k) <- path.(j - k);
    path.(j - k) <- t
  done

let is_opt ?(debug = false) adj_matrix path =
  let exception Not_opted in
  (* If [partial_path] is set to true the algorithm won't try to optimize the edge between the end of the path and the beginning.
     It's useful if you want to optimize the part of a path *)
  let bound = Array.length path in
  let bounded i = if i >= bound then i - bound else i in

  let rec loop1 i =
    if i >= bound - 3 then ()
    else (
      loop2 i (i + 1);
      loop1 (i + 1))
  and loop2 i j =
    if j >= bound - max 0 (1 - i) then ()
    else
      let diff =
        adj_matrix.(path.(i)).(path.(j))
        + adj_matrix.(path.(i + 1)).(path.(bounded (j + 1)))
        - adj_matrix.(path.(i)).(path.(i + 1))
        - adj_matrix.(path.(j)).(path.(bounded (j + 1)))
      in
      if diff < 0 then (
        if debug then
          Printf.printf "path not opted : could invert %d and %d" i j;
        raise Not_opted);
      loop2 i (j + 1)
  in
  try
    loop1 0;
    true
  with Not_opted -> false

exception Timed_Out

let opt_fast ?(debug = false) ?(partial_path = false) ?(max_iter = -1)
    ?(max_time = infinity) ?(lower_bound = 0) ?(upper_bound = -1)
    ?(check_time = 1000) adj_matrix path =
  (* If [partial_path] is set to true the algorithm won't try to optimize the edge between the end of the path and the beginning.
     It's useful if you want to optimize the part of a path *)
  let bound =
    if upper_bound < 0 then Array.length path
    else min upper_bound @@ Array.length path
  in
  let bounded i = if i >= bound then i - bound else i in
  let partial = if partial_path then 1 else 0 in
  let last_time_check = ref 0 in
  let start_time = if max_time = infinity then 0. else Unix.gettimeofday () in

  (* SI max_time = infinity ne pas appeler Unix.gettimeofday () *)
  let rec rec_while i =
    (i < max_iter || max_iter < 0)
    && (not (loop1 lower_bound true))
    && rec_while (i + 1)
  and loop1 i is_opt =
    if i >= bound - 3 - partial then is_opt
    else loop1 (i + 1) (loop2 i (i + 2) is_opt)
  and loop2 i j is_opt =
    if
      max_time <> infinity
      && (if !last_time_check > check_time then (
          last_time_check := 0;
          true)
         else (
           incr last_time_check;
           false))
      && Unix.gettimeofday () -. start_time > max_time
    then raise Timed_Out;
    if j >= bound - max (2 * partial) (1 - i) then is_opt
    else
      let diff =
        adj_matrix.(path.(i)).(path.(j))
        + adj_matrix.(path.(i + 1)).(path.(bounded (j + 1)))
        - adj_matrix.(path.(i)).(path.(i + 1))
        - adj_matrix.(path.(j)).(path.(bounded (j + 1)))
      in
      if diff < 0 then (
        invertPath i j path;
        if debug then Printf.printf "\ninverted %d and %d, diff : %d" i j diff);
      loop2 i (j + 1) (is_opt && diff >= 0)
  in

  try
    let is_opt = loop1 lower_bound true in
    (if not is_opt then
     let _ = rec_while 1 in
     ());
    is_opt
  with Timed_Out -> false

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

let opt_first ?(debug = false) ?(partial_path = false) ?(max_iter = -1)
    ?(max_time = infinity) ?(lower_bound = 0) ?(upper_bound = -1)
    ?(check_time = 1000) adj_matrix path =
  (* If [partial_path] is set to true the algorithm won't try to optimize the edge between the end of the path and the beginning.
     It's useful if you want to optimize the part of a path *)
  let bound =
    if upper_bound < 0 then Array.length path
    else min upper_bound @@ Array.length path
  in
  let bounded i = if i >= bound then i - bound else i in
  let partial = if partial_path then 1 else 0 in
  let last_time_check = ref 0 in
  let start_time = if max_time = infinity then 0. else Unix.gettimeofday () in
  (* SI max_time = infinity ne pas appeler Unix.gettimeofday () *)
  let rec rec_while i =
    (i < max_iter || max_iter < 0)
    && (not (loop1 lower_bound))
    && rec_while (i + 1)
  and loop1 i = i >= bound - 3 - partial || (loop2 i (i + 2) && loop1 (i + 1))
  and loop2 i j =
    if
      max_time <> infinity
      && (if !last_time_check > check_time then (
          last_time_check := 0;
          true)
         else (
           incr last_time_check;
           false))
      && Unix.gettimeofday () -. start_time > max_time
    then raise Timed_Out;
    j >= bound - max (2 * partial) (1 - i)
    || (let diff =
          adj_matrix.(path.(i)).(path.(j))
          + adj_matrix.(path.(i + 1)).(path.(bounded (j + 1)))
          - adj_matrix.(path.(i)).(path.(i + 1))
          - adj_matrix.(path.(j)).(path.(bounded (j + 1)))
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
