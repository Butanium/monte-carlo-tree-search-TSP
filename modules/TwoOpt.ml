module TwoOpt = struct
    let invertPath i j path =
        for k = 0 to (j - i)/2 - 1 do
             let t = path.(i+1+k) in
             path.(i+1+k) <- path.(j-k);
             path.(j-k) <- t;
         done

    let opt_best ?(debug = false) ?(partial_path = false) ?(maxi = -1) eval path =
        let bound = Array.length path in
        let partial = if partial_path then 1 else 0 in
        let rec loop k =
            let diff = ref 0 in
            let minI, minJ = ref 0, ref 0 in
                for i = 0 to bound - 4 - partial do
                    for j = i+2 to bound - 1 - max (2*partial) (1-i) do
                        let d = eval path.(i) path.(j) + eval path.(i+1) path.((j+1) mod bound)
                        - eval path.(i) path.(i+1) - eval path.(j) path.((j+1) mod bound)
                        in
                        if d < !diff then (
                            diff := d;
                            minI := i;
                            minJ := j
                        )
                    done
                done;
            if !diff < 0 then (
                invertPath !minI !minJ path;
                if debug then Printf.printf "\ninverted %d and %d" !minI !minJ;
                if k < maxi || maxi < 0 then loop (k+1)
            )
        in loop 1

    let opt_fast ?(debug = false) ?(partial_path = false) ?(maxi = -1) eval path =
        let bound = Array.length path in
        let partial = if partial_path then 1 else 0 in
        let rec rec_while i = (i < maxi || maxi < 0) &&
            not (loop1 0) && rec_while (i+1)
        and loop1 i = i >= bound - 3 - partial  || (loop2 i (i+2) && loop1 (i+1))
        and loop2 i j = j >= bound - max (2*partial) (1-i)  || (
            let diff = eval path.(i) path.(j) + eval path.(i+1) path.((j+1) mod bound)
                                   - eval path.(i) path.(i+1) - eval path.(j) path.((j+1) mod bound)  in
            if diff < 0 then (
                invertPath i j path;
                if debug then Printf.printf "\ninverted %d and %d, diff : %d" i j diff;
                false
            ) else true
        ) && loop2 i (j+1)
        in
        rec_while 0



    type random_creation = Roulette | Random

    let weight_update eval last q = function
        | Random -> ()
        | Roulette -> RndQ.change_weights (fun _ x -> 1. /. float_of_int(eval x last)) q

    let random_path q eval mode city_count =
        Array.init city_count (
            fun _ -> let v = RndQ.take q in
                weight_update eval v q mode;
                v
        )

    let iter_two_opt n eval city_count rnd_mode =
        let arr = Array.init city_count (Fun.id) in
        let q = RndQ.simple_create city_count arr in
        let best_len = ref max_int in
        let best_path = Array.make city_count (-1) in
        for _ = 1 to n do
            let path = random_path q eval rnd_mode city_count in
            opt_fast eval path;
            let len = Basetsp.path_length eval path in
            if len < !best_len then (
                best_len := len;
                for i = 0 to city_count -1 do
                    best_path.(i) <- path.(i)
                done
            );
            RndQ.reset q

        done;
        best_path

end;;
