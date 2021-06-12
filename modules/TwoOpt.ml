module TwoOpt = struct
    let invertPath i j path =
        for k = 0 to (j - i)/2 - 1 do
             let t = path.(i+1+k) in
             path.(i+1+k) <- path.(j-k);
             path.(j-k) <- t;
         done
    let opt_best ?(debug = false) ?(max = -1) eval path =
        let bound = Array.length path - 1 in

        let rec loop k =
            let diff = ref 0. in
            let minI, minJ = ref 0, ref 0 in
                for i = 0 to bound - 4 do
                    for j = i+2 to bound - 2 do
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
                if debug then Printf.printf "\ninverted %d and %d" !minI !minJ;
                if k < max || max < 0 then loop (k+1)
            )
        in loop 1
    let opt_fast ?(debug = false) ?(max = -1) eval path =
        let bound = Array.length path - 1 in
        let rec rec_while i = (i < max || max < 0) &&
            not (loop1 0) && rec_while (i+1)
        and loop1 i = i >= bound - 4 || (loop2 i (i+2) && loop1 (i+1))
        and loop2 i j = j >= bound - 1 || (
            if eval path.(i) path.(j) +. eval path.(i+1) path.(j+1)
            -. eval path.(i) path.(i+1) -. eval path.(j) path.(j+1) < 0. then (
                invertPath i j path;
                if debug then Printf.printf "\ninverted %d and %d" i j;
                false
            ) else true
        ) && loop2 i (j+1)
        in
        let _ = rec_while 0 in ()
end;;
