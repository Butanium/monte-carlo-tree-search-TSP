module TwoOpt = struct
    let invertPath i j path =
        for k = 0 to (j - i)/2 - 1 do
             let t = path.(i+1+k) in
             path.(i+1+k) <- path.(j-k);
             path.(j-k) <- t;
         done
    let opt_best city_count eval max path =
        let rec loop k =
            let diff = ref 0. in
            let minI, minJ = ref 0, ref 0 in
                for i = 0 to city_count - 4 do
                    for j = i+2 to city_count - 2 do
                        let d = eval path.(i) path.(j) +. eval path.(i+1) path.(j+1)
                        -. eval path.(i) path.(i+1) -. eval path.(j) path.(j+1)
                        in
                        if d < !diff then (
                            diff := d;
                            minI := i;
                            minJ := j
                        )
                    done
                done;
            if !diff < 0. then (
                invertPath !minI !minJ path;
                if k < max || max < 0 then loop (k+1)
            )
        in loop 1
    let opt_fast city_count eval max path =
        let rec rec_while i = (i < max || max < 0) &&
            not (loop1 0) && rec_while (i+1)
        and loop1 i = i >= city_count - 4 || (loop2 i (i+2) && loop1 (i+1))
        and loop2 i j = j >= city_count - 1 || (
            if eval path.(i) path.(j) +. eval path.(i+1) path.(j+1)
            -. eval path.(i) path.(i+1) -. eval path.(j) path.(j+1) < 0. then (
                invertPath i j path;
                Printf.printf "\ninverted %d and %d" i j;
                false
            ) else true
        ) && loop2 i (j+1)
        in
        let _ = rec_while 0 in ()
end;;