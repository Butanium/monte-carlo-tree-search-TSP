module TwoOpt = struct
    type arg = {city_count: int; eval : int -> int -> float}
    let optBest city_count eval max arr =
        let rec loop k =
            let diff = ref 0. in
            let minI, minJ = ref 0, ref 0 in
                for i = 0 to arg.city_count - 4 do
                    for j = i+2 to arg.city_count - 2 do
                        let d = arg.eval arr.(i) arr.(j) +. arg.eval arr.(i+1) arr.(j+1)
                        -. arg.eval arr.(i) arr.(i+1) -. arg.eval arr.(j) arr.(j+1)
                        in
                        if d < !diff then (
                            diff := d;
                            minI := i;
                            minJ := j
                        )
                    done
                done;
            if !diff < 0. then (
                let i, j = !minI, !minJ in
                for k = 0 to (j - i)/2 - 1 do
                    let t = arr.(i+1+k) in
                    arr.(i+1+k) <- arr.(j-k);
                    arr.(j-k) <- t;
                done;
                if k < max || max < 0 then loop (k+1)
            )
        in loop 1



end;;