module RndQ = Random_Queue

type two_opt_type = Fast | Best | First

let string_of_opt_policy = function
  | Fast -> "Fast"
  | First -> "First"
  | Best -> "Best"

let max (x : int) (y : int) = if x < y then y else x

let invertPath i j tour =
  for k = 0 to ((j - i) / 2) - 1 do
    let t = tour.(i + 1 + k) in
    tour.(i + 1 + k) <- tour.(j - k);
    tour.(j - k) <- t
  done

let is_opt ?(debug = false) adj_matrix tour =
  let exception Not_opted in
  (* If [partial_path] is set to true the algorithm won't try to optimize the edge between the end of the tour and the beginning.
     It's useful if you want to optimize the part of a tour *)
  let bound = Array.length tour in
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
        adj_matrix.(tour.(i)).(tour.(j))
        + adj_matrix.(tour.(i + 1)).(tour.(bounded (j + 1)))
        - adj_matrix.(tour.(i)).(tour.(i + 1))
        - adj_matrix.(tour.(j)).(tour.(bounded (j + 1)))
      in
      if diff < 0 then (
        if debug then
          Printf.printf "tour not opted : could invert %d and %d" i j;
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
    ?(check_time = 1000) adj_matrix tour =
  (* If [partial_path] is set to true the algorithm won't try to optimize the edge between the end of the tour and the beginning.
     It's useful if you want to optimize the part of a tour *)
  let bound =
    if upper_bound < 0 then Array.length tour
    else min upper_bound @@ Array.length tour
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
        adj_matrix.(tour.(i)).(tour.(j))
        + adj_matrix.(tour.(i + 1)).(tour.(bounded (j + 1)))
        - adj_matrix.(tour.(i)).(tour.(i + 1))
        - adj_matrix.(tour.(j)).(tour.(bounded (j + 1)))
      in
      if diff < 0 then (
        invertPath i j tour;
        if debug then Printf.printf "\ninverted %d and %d, diff : %d" i j diff);
      loop2 i (j + 1) (is_opt && diff >= 0)
  in

  try
    let is_opt = loop1 lower_bound true in
    if not is_opt then ignore (rec_while 1);
    is_opt
  with Timed_Out -> false

let opt_best ?(debug = false) ?(partial_path = false) ?(max_iter = -1)
    ?(max_time = infinity) ?(check_time = 1000) adj_matrix tour =
  let last_time_check = ref 0 in
  let start_time = if max_time = infinity then 0. else Unix.gettimeofday () in
  let bound = Array.length tour in
  let partial = if partial_path then 1 else 0 in
  let eval i j = adj_matrix.(i).(j) in
  let rec loop k =
    let diff = ref 0 in
    let minI, minJ = (ref 0, ref 0) in
    for i = 0 to bound - 4 - partial do
      for j = i + 2 to bound - 1 - max (2 * partial) (1 - i) do
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
        let d =
          eval tour.(i) tour.(j)
          + eval tour.(i + 1) tour.((j + 1) mod bound)
          - eval tour.(i) tour.(i + 1)
          - eval tour.(j) tour.((j + 1) mod bound)
        in
        if d < !diff then (
          diff := d;
          minI := i;
          minJ := j)
      done
    done;
    if !diff < 0 then (
      invertPath !minI !minJ tour;
      if debug then Printf.printf "\ninverted %d and %d" !minI !minJ;
      if k < max_iter || max_iter < 0 then loop (k + 1))
  in
  try loop 1 with Timed_Out -> ()

let opt_first ?(debug = false) ?(partial_path = false) ?(max_iter = -1)
    ?(max_time = infinity) ?(lower_bound = 0) ?(upper_bound = -1)
    ?(check_time = 1000) adj_matrix tour =
  (* If [partial_path] is set to true the algorithm won't try to optimize the edge between the end of the tour and the beginning.
     It's useful if you want to optimize the part of a tour *)
  let bound =
    if upper_bound < 0 then Array.length tour
    else min upper_bound @@ Array.length tour
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
          adj_matrix.(tour.(i)).(tour.(j))
          + adj_matrix.(tour.(i + 1)).(tour.(bounded (j + 1)))
          - adj_matrix.(tour.(i)).(tour.(i + 1))
          - adj_matrix.(tour.(j)).(tour.(bounded (j + 1)))
        in
        if diff < 0 then (
          invertPath i j tour;
          if debug then Printf.printf "\ninverted %d and %d, diff : %d" i j diff;
          false)
        else true)
       && loop2 i (j + 1)
  in
  try ignore (rec_while 0) with Timed_Out -> ()

let two_opt ?(debug = false) ?(partial_path = false) ?(max_iter = -1)
    ?(max_time = infinity) ?(lower_bound = 0) ?(upper_bound = -1)
    ?(check_time = 1000) opt_mode adj_matrix tour =
  match opt_mode with
  | Fast ->
      opt_fast ~debug ~partial_path ~max_iter ~max_time ~lower_bound
        ~upper_bound ~check_time adj_matrix tour
      |> ignore
  | First ->
      opt_first ~debug ~partial_path ~max_iter ~max_time ~lower_bound
        ~upper_bound ~check_time adj_matrix tour
  | Best ->
      opt_best ~debug ~partial_path ~max_iter ~max_time ~check_time adj_matrix
        tour
