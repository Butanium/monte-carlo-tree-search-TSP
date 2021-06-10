module MCTS = struct
    let mapInPlace f arr = Array.iteri (fun i x -> arr.(i) <- f x) arr
    

    module IntSet = Set.Make(Int)
    module RndQ = struct
            exception Empty
            type 'a t = {mutable size: int; content : ('a * float) array; mutable tot : float}
            (** [create size arr] *)
            let getTot q = 
                let t = ref 0. in 
                for k = 0 to q.size - 1 do 
                    t := !t +. let _,p = q.content.(k) in p 
                done;
                !t
            let simple_create size arr = 
                {size; content = Array.map (fun i -> i,1.) arr; tot = float_of_int size}
            let create size arr weights = let tot = Array.fold_left (+.) 0. weights in
                {size; content = Array.mapi (fun i x -> x, weights.(i)) arr; tot}
            let is_empty q = q.size = 0
            let take q = if q.size = 0 then raise Empty else
                let rec aux k acc = if k >= q.size - 1 then k else (
                    let acc = acc -. let _, p = q.content.(k) in p in
                            if acc <= 0. then k else aux (k+1) acc
                        )
                    in
                    let i = aux  0 @@ Random.float 1.*. getTot q  in
                        let res, p as r = q.content.(i) in
                    q.content.(i) <- q.content.(q.size - 1);
                    q.size <- q.size - 1;
                    res
            let tot_empty q =
                Array.init q.size (fun _ -> take q)
    end
    type baseNode = {mutable accScore:float; mutable visit:float; mutable sons: mctNode list;
            poss_cities: int RndQ.t; accDist:float; city:int}
    and heritage =  {used_cities: IntSet.t; init_score: float; init_length: int; start_path: int list}
    and herit_node = R of heritage | N of mctNode
    and mctNode = {base: baseNode; herit_node: herit_node}
    type creation_mode = Roulette | Random
    type opt_mode = No_opt | Two_Opt_Best of int | Two_Opt_Fast of int
    type select_mode = Ucb1 of float
    type arg = {city_count: int; eval : int -> int -> float; rnd_creation_mode: creation_mode; opt_mode: opt_mode;
                mutable score:float; start:int; mutable length:int; mutable visited:IntSet.t; mutable last_city:int;
                select_mode: select_mode}
    let get_father node = match node.herit_node with | R _ -> failwith "no dads for root" | N n -> n
    let node_score arg =
        match arg.select_mode with
        | Ucb1 e -> fun node -> let nf = (get_father node).base.visit in
                                e *. (log(nf) /. node.base.visit) ** 0.5  -. node.base.accScore /. node.base.visit
    let random_creation arg =
        match arg.rnd_creation_mode with
        | Roulette -> 100. /. arg.eval arg.last_city
        | Random -> fun _ -> 1.
        | _ -> failwith "not implemented"
    let playout_opt arg =
        match arg.opt_mode with
        | No_opt -> fun _ -> ()
        | Two_Opt_Best m -> TwoOpt.opt_best arg.city_count arg.eval m
        | Two_Opt_Fast m -> TwoOpt.opt_fast arg.city_count arg.eval m
   
    let available arg =
       let av_count = arg.city_count - arg.length
       in 
       let arr = Array.make av_count (-1) in
       let fill = let ind = ref 0 in
           fun x -> arr.(!ind) <- x; incr ind
       in
       let rec aux i accC =
           if not (i = arg.city_count || accC = av_count)
               then (
                   let b = IntSet.mem i arg.visited in if b then fill i;
                   aux (i+1) (accC + if b then 0 else 1)
               )
       in aux 0 0;
       RndQ.create av_count arr @@ Array.init av_count (fun i -> random_creation arg arr.(i))
    let playout arg =
        let av_count = arg.city_count - arg.length in
        let q = available arg in
        let end_path = RndQ.tot_empty q
        in
        playout_opt arg end_path;
        let r = ref @@ arg.score +. arg.eval arg.last_city end_path.(0) +. arg.eval end_path.(av_count - 1) arg.start
        in
        for i = 1 to av_count - 2 do
            r := !r +. arg.eval end_path.(i) end_path.(i+1)
        done;
        !r
    let rec retropropagation node value =
        node.base.visit <- node.base.visit +. 1.;
        node.base.accScore <- value +. node.base.accScore;
        match node.herit_node with
            | N father -> retropropagation father value
            | _ -> ()

    let expend node arg =
        try (
        let x = RndQ.take node.base.poss_cities in
            let dist = arg.eval node.base.city x
            in
            arg.score <- arg.score +. dist;
            arg.length <- arg.length + 1;
            arg.visited <- IntSet.add x arg.visited;
            arg.last_city <- x;
            let newN = {base={accScore=0.; visit=0.; sons = []; poss_cities = available arg;
                    accDist = node.base.accDist +. dist; city=x}; herit_node = N node}
            in
            node.base.sons <- newN :: node.base.sons;
            retropropagation newN @@ playout arg
        ) with Failure e -> failwith @@ "expend failed : "^e
    
    let rec select node arg =
        match RndQ.is_empty node.base.poss_cities, node.base.sons with 
            | true, [] -> retropropagation node @@ arg.score +. arg.eval node.base.city arg.last_city
            | false, _ -> expend node arg
            | true, x :: xs -> let score = node_score arg in
                let _, n = List.fold_left
                     (fun ((accS, accN) as acc) n -> let s =  score n in if s > accS then s, n else acc) (score  x, x) xs
                     in select n arg
    (*mcts city_count eval rnd_creation_mode opt_mode select_mode min_conv max_playout max_time*)
    let mcts city_count eval rnd_creation_mode opt_mode select_mode min_conv max_playout max_time =
        let arg = {city_count; eval; rnd_creation_mode; opt_mode; score = 0.; start = 0; length = 1;
            visited = IntSet.singleton 0; last_city=0; select_mode}
        in
        let poss_cities = available arg
        in
        let root = ref {base={accScore=0.; visit = 0.; sons = []; poss_cities; accDist = 0.; city=0};
            herit_node = R{used_cities = IntSet.singleton 0; init_score=0.; init_length=1; start_path = [0]}}
        in
        let get_best_son node =
            match node.base.sons with
            | x :: xs -> List.fold_left
                        (fun ((accS, accN) as acc) n -> let s = n.base.accScore /. n.base.visit in if s < accS then s, n else acc)
                        (x.base.accScore /. x.base.visit, x) xs
            | _ -> failwith "no sons"
        in
        let getRatio() =
            let r = List.fold_left (fun acc n -> max acc n.base.visit) 0. (!root).base.sons
            in r /. (!root.base.visit)
        in
        let getRoot() = match !root.herit_node with | R x -> x | _ -> failwith "Invalid root"
        in
        while (getRoot()).init_length < city_count do
            let t = Sys.time() in
            while (!root).base.visit < float_of_int max_playout && getRatio() < min_conv && Sys.time() -. t < max_time do
                let r = getRoot() in
                arg.score <- r.init_score;
                arg.length <- r.init_length;
                arg.visited <- r.used_cities;
                arg.last_city <- (!root).base.city;
                select !root arg;
            done;
            let _, n = get_best_son !root in
            let r = getRoot() in
            let b_root = (!root).base in
            let used_cities = IntSet.add n.base.city r.used_cities in
            let init_score = r.init_score +. arg.eval b_root.city n.base.city in
            let start_path = n.base.city :: r.start_path in
            let init_length = r.init_length + 1 in
            let newRoot = {base=n.base; herit_node=R {used_cities; init_score; init_length; start_path}}
            in root := newRoot
            (* update root *)
        done;
        List.rev @@ arg.start :: (getRoot()).start_path












end;;