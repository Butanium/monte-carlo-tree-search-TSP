module MCTS2 = struct

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
            let take q = try(
                if q.size = 0 then raise Empty else
                let rec aux k acc = if k >= q.size - 1 then k else (
                    let acc = acc -. let _, p = q.content.(k) in p in
                            if acc <= 0. then k else aux (k+1) acc
                        )
                    in
                    let i = aux  0 @@ Random.float 1.*. getTot q  in
                        let res, p as r = q.content.(i) in
                    q.content.(i) <- q.content.(q.size - 1);
                    q.size <- q.size - 1;
                    res) with Invalid_argument e -> raise @@ Invalid_argument ("take failed : "^e)
            let tot_empty q =
                Array.init q.size (fun _ -> take q)
    end
    type baseNode = {mutable accScore:float; mutable visit:float; mutable sons: mctNode list;
            poss_cities: int RndQ.t; accDist:float; city:int}
    and heritage =  {used_cities: IntSet.t; init_score: float; init_length: int; start_path: int list}
    and herit_node = R of heritage | N of mctNode
    and mctNode = {base: baseNode; herit_node: herit_node}
    type creation_mode = Roulette | Random
    type opt_mode = No_opt | Two_opt_best | Two_opt_fast
    type select_mode = Ucb1 of float | Ucb1_mst of float | Ucb1_ecart_type of float
    type arg = {debug: bool; city_count: int; eval : int -> int -> float; rnd_creation_mode: creation_mode; opt_mode: opt_mode;
                start:int; mutable length:int; mutable visited:IntSet.t; mutable last_city:int;
                mutable select_mode: select_mode; mutable min_dist:float; current_path:int array;
                mutable best_path : int list}
    let get_father node = match node.herit_node with | R _ -> failwith "no dads for root" | N n -> n
    let node_score arg =
        match arg.select_mode with
        | Ucb1 e -> fun node -> let nf = (get_father node).base.visit in
                                e *. (log(nf) /. node.base.visit) ** 0.5  -. node.base.accScore /. node.base.visit
        | Ucb1_mst e -> let inf = Primalg.primalg arg.eval arg.city_count in
                    fun node -> let nf = (get_father node).base.visit in
                                inf *. e *. (log(nf) /. node.base.visit) ** 0.5  -. node.base.accScore /. node.base.visit
        | Ucb1_ecart_type e -> failwith "unexpected ucb1 mode, ecart_type need to be converted into ucb1"
    let random_creation arg =
        match arg.rnd_creation_mode with
        | Roulette -> fun c -> 1. /. arg.eval arg.last_city c
        | Random -> fun _ -> 1.
        | _ -> failwith "not implemented"
    let playout_opt arg =
        match arg.opt_mode with
        | No_opt -> fun _ -> ()
        | Two_opt_best -> TwoOpt.opt_best arg.eval
        | Two_opt_fast -> TwoOpt.opt_fast arg.eval

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
                   let b = not @@ IntSet.mem i arg.visited in if b then fill i;
                   aux (i+1) (accC + if b then 1 else 0)
               )
       in aux 0 0;
       RndQ.create av_count arr @@ Array.init av_count (fun i -> random_creation arg arr.(i))

    let playout arg init_dist =
        let av_count = arg.city_count - arg.length in
        if av_count = 0 then arg.eval arg.last_city arg.start else (
            let q = available arg in
            let end_path = RndQ.tot_empty q
            in
            (
            try (
            playout_opt arg end_path;
            ) with Invalid_argument e -> raise @@ Invalid_argument ("opt failed :"^e));
            let r = ref @@ init_dist +. arg.eval arg.last_city end_path.(0) +. arg.eval end_path.(av_count - 1) arg.start
            in

            for i = 0 to av_count - 2 do
                r := !r +. arg.eval end_path.(i) end_path.(i+1)
            done;

            if !r < arg.min_dist then (
                arg.min_dist <- !r;
                let rec aux i acc = if i = arg.city_count then arg.start :: acc else
                    let n = if i >= arg.length then end_path.(i - arg.length)
                        else arg.current_path.(i)
                    in
                    aux (i+1) (n :: acc)
                in
                arg.best_path <- aux 1 []
            );

            !r
        )
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
            arg.current_path.(arg.length) <- x;
            arg.visited <- IntSet.add x arg.visited;
            arg.last_city <- x;
            arg.length <- arg.length + 1;
            let accDist = node.base.accDist +. dist in
            let newN = {base={accScore=0.; visit=0.; sons = []; poss_cities = available arg;
                    accDist; city=x}; herit_node = N node}
            in
            node.base.sons <- newN :: node.base.sons;
            try (retropropagation newN @@ playout arg accDist) with Invalid_argument e -> raise @@ Invalid_argument ("PLAYOUT"^e)
        ) with Invalid_argument e -> raise @@ Invalid_argument ("expend failed : "^e)

    let rec select node arg =
        let update_arg arg n =
                arg.current_path.(arg.length) <- n.base.city;
                arg.length <- arg.length + 1;
                assert (not @@ IntSet.mem n.base.city arg.visited);
                arg.visited <- IntSet.add n.base.city arg.visited;
                arg.last_city <- n.base.city
        in
        match RndQ.is_empty node.base.poss_cities, node.base.sons with
            | true, [] -> (try (
                arg.length <- arg.length + 1;
                let s = node.base.accScore +. arg.eval node.base.city arg.start in
                if s < arg.min_dist then (
                    arg.min_dist <- s;
                    arg.current_path.(arg.city_count - 1) <- node.base.city;
                    arg.best_path <- Array.to_list arg.current_path;
                    assert (abs_float (s -. Basetsp.path_length arg.eval arg.current_path) < 0.0001)
                );
                retropropagation node s)
                with Invalid_argument e -> raise @@ Invalid_argument ("retropropagation failed : "^e))
            | false, _ -> (try (update_arg arg node; expend node arg) with Invalid_argument e -> raise @@ Invalid_argument ("expend failed : "^e))
            | true, x :: xs -> (let score = node_score arg in
                let _, n = List.fold_left
                     (fun ((accS, accN) as acc) n -> let s =  score n in if s > accS then s, n else acc) (score  x, x) xs
                     in update_arg arg node; select n arg)
    (*mcts city_count eval rnd_creation_mode opt_mode select_mode min_conv max_playout max_time*)
    let debug_node node =
        Printf.printf "visits : %.0f, dist ratio : %.1f, city : %d, not developped : %d\n"
            node.base.visit (node.base.accScore /. node.base.visit) node.base.city node.base.poss_cities.size

    let mcts ?(debug=false) city_count eval rnd_creation_mode opt_mode select_mode max_playout max_time =
        let arg = {debug; city_count; eval; rnd_creation_mode; opt_mode; start = 0; length = 1;
            visited = IntSet.singleton 0; last_city=0; select_mode; min_dist=infinity;
            current_path= Array.make city_count (-1); best_path=[]}
        in
        let poss_cities = available arg
        in
        let root = {base={accScore=0.; visit = 0.; sons = []; poss_cities; accDist = 0.; city=0};
            herit_node = R{used_cities = IntSet.empty; init_score=0.; init_length=0; start_path = [0]}}
        in
        let get_best_son node =
            match node.base.sons with
            | x :: xs -> List.fold_left
                        (fun ((accS, accN) as acc) n -> let s = n.base.accScore /. n.base.visit in if s < accS then s, n else acc)
                        (x.base.accScore /. x.base.visit, x) xs
            | _ -> failwith "no sons"
        in
        let getRoot() = match root.herit_node with | R x -> x | _ -> failwith "Invalid root"
        in
        let playout_count() = root.base.visit
        in
        let max_playout = float_of_int max_playout in
        arg.current_path.(0) <- 0;
        let t = Sys.time() in
        let max_depth = ref 0 in
        while playout_count() < max_playout && Sys.time() -. t < max_time do
            let r = getRoot() in
            max_depth := max !max_depth arg.length;
            arg.length <- r.init_length;
            arg.visited <- r.used_cities;
            arg.last_city <- (root).base.city;
            (match RndQ.is_empty (root).base.poss_cities, arg.select_mode with
            | true, Ucb1_ecart_type e -> let moy = List.fold_left (fun acc n -> acc +. n.base.accScore /. n.base.visit) 0. (root).base.sons
                in arg.select_mode <- Ucb1 (e *. moy)
            | _ -> ());
            (try (
            select root arg;
            ) with Invalid_argument e -> raise @@ Invalid_argument ("SELECT failed: "^e));
            assert (abs_float (arg.min_dist -. Basetsp.path_length arg.eval (Array.of_list arg.best_path)) < 0.0001)
        done;
        Printf.printf "%.0f playouts done, max depth : %d\n" root.base.visit !max_depth;
        if (not (playout_count() < max_playout)) then print_endline "finished by max_playout\n";
        if (not (Sys.time() -. t < max_time)) then print_endline "finished by time\n";
        print_endline "root sons : ";
                List.iter debug_node root.base.sons;
        let r = Array.of_list arg.best_path in
        (* TwoOpt.opt_fast arg.eval r; *)
        r

end;;

