module MCTS = struct
    let mapInPlace f arr = Array.iteri (fun i x -> arr.(i) <- f x) arr
    module IntSet = Set.Make(Int)
    module RndQ = struct
        exception Empty
        type 'a t = {mutable size: int; content : 'a array; weights : float array; mutable tot : float}
        (** [create size arr] *)
        let getTot q = let t = ref 0 in for k = 0 to q.size - 1 do t := !t +. weights.(content.(k)) done;
        
        let create size arr weights = let tot = Array.fold_left (+.) 0. weights in
                 Array.{size; content = arr; weights; tot}
        let is_empty q = q.size = 0
        let take t = if t.size = 0 then raise Empty else
            let rec aux k acc = let acc = acc -. t.weights.(k) in
                if k = t.size - 1 || acc <= 0. then k else aux (k+1) acc
            in
            let i = aux 0 @@ Random.float 1.  in
            let r = t.content.(i) in
            t.content.(i) <- t.content.(t.size - 1);
            t.size <- t.size - 1;
            r
    end
    type arg = {city_count: int; eval : int -> int -> float}
    let playout arg score start visited len =
        let av_count = arg.city_count - len in
        let arr = Array.make av_count (-1) in
        let fill = let ind = ref 0 in
            fun x -> arr.(!ind) <- x; incr ind
        in

        let available =
            let rec aux i accC =
                if not @@ i = arg.city_count || accC = av_count
                    then (
                        let b = IntSet.mem i visited in if b then fill i;
                        aux (i+1) (accC + if b then 0 else 1)
                    )
            in aux 0 0
        in
        RndQ.create av_count arr @@ Array.init av_count (fun i -> arg.eval arr.(i) start)








end