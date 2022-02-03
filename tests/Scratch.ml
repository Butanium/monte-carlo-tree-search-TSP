let start_time = Unix.gettimeofday () in
for i = 1 to 10000000 do
    ignore (Sys.time ())
done;
let end_time = Unix.gettimeofday () in
Printf.printf "Sys.time %f\n" (end_time -. start_time);

let start_time = Unix.gettimeofday () in
for i = 1 to 10000000 do
    ignore (Unix.gettimeofday ())
done;
let end_time = Unix.gettimeofday () in
Printf.printf "Unix.gettimeofday %f\n" (end_time -. start_time)