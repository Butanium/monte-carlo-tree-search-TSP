(* todo remove assert for runtime *)
module MCTS2 = struct

    type baseNode = {mutable accScore:float; mutable visit:float; mutable childs: mctNode list;
            poss_cities: int RndQ.t; accDist:int; city:int}
    and heritage =  {used_cities: IntSet.t; init_score: float; init_length: int; start_path: int list}
    and herit_node = R of heritage | N of mctNode
    and mctNode = {base: baseNode; herit_node: herit_node}
    type average_mode = Arithmetic | Geometric | Harmonic
    type creation_mode = Roulette | Random
    type opt_mode = No_opt | Two_opt_best | Two_opt_fast
    type select_mode = Ucb1 of float | Ucb1_deviation of float | Ucb1_mst of float | Ucb1_average of float
    type arg = {debug: bool; city_count: int; eval : int -> int -> int; rnd_creation_mode: creation_mode; opt_mode: opt_mode;
                start:int; mutable length:int; mutable visited:IntSet.t; mutable last_city:int;
                mutable node_score: mctNode -> float; average_mode: average_mode; mutable min_dist:int; current_path:int array;
                mutable best_path : int list}
    let get_father node = match node.herit_node with | R _ -> failwith "no dads for root" | N n -> n


    let random_creation arg =
        match arg.rnd_creation_mode with
        | Random -> fun _ -> 1.
        | Roulette -> fun c -> 1. /. float_of_int (arg.eval arg.last_city c)


    let weight_update arg last q =
        match arg.rnd_creation_mode with
        | Random ->()
        | Roulette -> RndQ.change_weights (fun _ x -> 1. /. float_of_int (arg.eval x last)) q

    let playout_opt arg =
        match arg.opt_mode with
        | No_opt -> fun _ -> ()
        | Two_opt_best -> TwoOpt.opt_best arg.eval
        | Two_opt_fast -> TwoOpt.opt_fast arg.eval

    let add_acc arg =
        match arg.average_mode with
            | Arithmetic -> fun acc x -> acc +. float_of_int x
            | Geometric -> fun acc x -> acc *. float_of_int x
            | Harmonic -> fun acc x -> acc +. 1. /. float_of_int x

    let get_average_fun average_mode node =
        let s = node.base.accScore in
        let n = node.base.visit in
        match average_mode with
            | Arithmetic -> s /. n
            | Geometric -> s ** (1. /. n)
            | Harmonic -> n /. s

    let get_node_score_fun average_mode eval city_count =
        let av = get_average_fun average_mode in
        function

        | Ucb1 e -> fun node -> let nf = (get_father node).base.visit in
                                e *. (log(nf) /. node.base.visit) ** 0.5  -. av node
        | Ucb1_mst e -> let inf = float_of_int @@  Primalg.primalg eval city_count in
                    fun node -> let nf = (get_father node).base.visit in
                                inf *. e *. (log(nf) /. node.base.visit) ** 0.5  -. av node
        | Ucb1_average _ | Ucb1_deviation _-> fun _ -> failwith "unexpected ucb1 mode, ecart_type need to be converted into ucb1"

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
        if av_count = 0 then init_dist + arg.eval arg.last_city arg.start else (
            let q = available arg in
            let end_path = Array.init av_count (
                fun _ -> let v = RndQ.take q in
                weight_update arg v q;
                v
                )
            in
            (
            try (
            playout_opt arg end_path;
            ) with Invalid_argument e -> raise @@ Invalid_argument ("opt failed :"^e));
            let r = ref @@ init_dist + arg.eval arg.last_city end_path.(0) + arg.eval end_path.(av_count - 1) arg.start
            in

            for i = 0 to av_count - 2 do
                r := !r + arg.eval end_path.(i) end_path.(i+1)
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
    let rec retropropagation node value arg =
        node.base.visit <- node.base.visit +. 1.;
        node.base.accScore <- (add_acc arg) node.base.accScore value;
        match node.herit_node with
            | N father -> retropropagation father value arg
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
            let accDist = node.base.accDist + dist in
            let newN = {base={accScore=0.; visit=0.; childs = []; poss_cities = available arg;
                    accDist; city=x}; herit_node = N node}
            in
            node.base.childs <- newN :: node.base.childs;
            try (retropropagation newN (playout arg accDist) arg ) with Invalid_argument e -> raise @@ Invalid_argument ("PLAYOUT"^e)
        ) with Invalid_argument e -> raise @@ Invalid_argument ("expend failed : "^e)

    let rec select node arg =
        let update_arg arg n =
                arg.current_path.(arg.length) <- n.base.city;
                arg.length <- arg.length + 1;
                arg.visited <- IntSet.add n.base.city arg.visited;
                arg.last_city <- n.base.city
        in
        match RndQ.is_empty node.base.poss_cities, node.base.childs with
            | true, [] -> (try (
                arg.length <- arg.length + 1;
                let s = node.base.accDist + arg.eval node.base.city arg.start in
                if s < arg.min_dist then (
                    arg.min_dist <- s;
                    arg.current_path.(arg.city_count - 1) <- node.base.city;
                    arg.best_path <- Array.to_list arg.current_path;
                    assert (s = Basetsp.path_length arg.eval arg.current_path)
                );
                retropropagation node s arg)
                with Invalid_argument e -> raise @@ Invalid_argument ("retropropagation failed : "^e))
            | false, _ -> (try (update_arg arg node; expend node arg) with Invalid_argument e -> raise @@ Invalid_argument ("expend failed : "^e))
            | true, x :: xs -> (let score = arg.node_score in
                let _, n = List.fold_left
                     (fun ((accS, accN) as acc) n -> let s =  score n in if s > accS then s, n else acc) (score x, x) xs
                     in update_arg arg node; select n arg)
    let debug_node node =
        Printf.printf "visits : %.0f, dist ratio : %.1f, city : %d, not developped : %d\n"
            node.base.visit (node.base.accScore /. node.base.visit) node.base.city node.base.poss_cities.size
    let get_best_son node =
        match node.base.childs with
        | x :: xs -> List.fold_left
                    (fun ((accS, accN) as acc) n -> let s = n.base.accScore /. n.base.visit in if s < accS then s, n else acc)
                    (x.base.accScore /. x.base.visit, x) xs
        | _ -> failwith "no childs"

    let rec debug_mcts root =
        print_endline "\nchosen : \n";
        debug_node root;
        print_endline "\nroot childs : \n";
        match root.base.childs with
        | [] -> ()
        | l -> begin
            List.iter (fun n -> (match n.herit_node with R _ -> () | N f ->
                Printf.printf "conv : %.1f%%  |  " @@ 100. *. n.base.visit /. f.base.visit); debug_node n;) @@ List.sort (fun n1 n2 -> - compare n1.base.visit n2.base.visit) l;
            debug_mcts @@ let _, n = get_best_son root in n;
        end




    let mcts ?(debug=false) city_count eval rnd_creation_mode opt_mode select_mode average_mode max_playout max_time =
        let arg = {debug; city_count; eval; rnd_creation_mode; opt_mode; start = 0; length = 1;
            visited = IntSet.singleton 0; last_city=0; average_mode; min_dist=max_int;
            node_score=get_node_score_fun average_mode eval city_count select_mode;
            current_path= Array.make city_count (-1); best_path=[]}
        in
        let poss_cities = available arg
        in
        let root = {base={accScore=0.; visit = 0.; childs = []; poss_cities; accDist = 0; city=0};
            herit_node = R{used_cities = IntSet.empty; init_score=0.; init_length=0; start_path = [0]}}
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
            (match RndQ.is_empty (root).base.poss_cities, select_mode with
            | true, Ucb1_average e -> 
                let moy = List.fold_left (fun acc n -> acc +. n.base.accScore) 0. (root).base.childs /. float_of_int (arg.city_count - 1)
                in arg.node_score <- get_node_score_fun average_mode eval city_count (Ucb1 (e *. moy))
            | true, Ucb1_deviation e -> let tot = float_of_int ( arg.city_count - 1 ) in
                let average = List.fold_left (fun acc node -> acc +. node.base.accScore) 0. root.base.childs /. tot in
                let c = (List.fold_left (fun acc node -> acc +. (node.base.accScore -. average) ** 2.) 0. root.base.childs /. tot) ** 0.5
                in arg.node_score <- get_node_score_fun average_mode eval city_count (Ucb1 (e *. c))
            | _ -> ());
            (try (
            select root arg;
            ) with Invalid_argument e -> raise @@ Invalid_argument ("SELECT failed: "^e));
            assert (arg.min_dist = Basetsp.path_length arg.eval (Array.of_list arg.best_path))
        done;
        Printf.printf "%.0f playouts done in %.0f seconds, max depth : %d, best score : %d\n"
            root.base.visit (Sys.time() -. t) !max_depth arg.min_dist;
        if (not (playout_count() < max_playout)) then print_endline "finished by max_playout\n";
        if (not (Sys.time() -. t < max_time)) then print_endline "finished by time\n";
        print_endline "root childs : ";
                List.iter debug_node root.base.childs;
        let r = Array.of_list arg.best_path in
        (* TwoOpt.opt_fast arg.eval r; *)
        (*debug_mcts root;*)
        r

end;;
