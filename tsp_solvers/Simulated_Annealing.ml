type random_change_mode = Swap | Insert | Invert

type argument = {mutable problem_size : float; mutable city_count : int; mutable solution : int array; mutable eval : int -> int -> int;
    mutable i : int; mutable j : int}

let arg = {problem_size = -1.; solution = [||]; eval = (fun _ _ -> -1); i = 0; j = 1; city_count = -1}

let random_choice() =
    let rnd shift = int_of_float @@ Random.float arg.problem_size +. 0.5 -. shift in
    let i = rnd 0. in
    let j = rnd 1. in
    let j = if j = i then int_of_float @@ arg.problem_size +. 0.5 else j in
    arg.i <- i;
    arg.j <- j

let get_delta_fun mode =
    let m = function
        | -1 -> arg.city_count - 1
        | x when x = arg.city_count -> 0
        | x -> x
    in
    let edge_len x y = arg.eval arg.solution.(x) arg.solution.(y)
    in
    fun () -> let i, j = arg.i, arg.j in
    match mode with
        | Swap -> edge_len (m @@ i-1) j + edge_len (m @@ i+1) j + edge_len (m @@ j-1) i + edge_len (m @@ j+1) i
                - edge_len (m @@ i-1) i - edge_len (m @@ i+1) i - edge_len (m @@ j-1) j - edge_len (m @@ j+1) j
        | _ -> failwith "not implemented yet"

let get_randomize_fun mode =
    fun () ->
    match mode with
        | Swap -> let m = arg.solution.(arg.i) in
                    arg.solution.(arg.i) <- arg.solution.(arg.j);
                    arg.solution.(arg.j) <- m;
        | _ -> failwith "not implemented yet"


let start_sa city_count eval init_T final_T iter_count cooling_ratio rnd_mode =
    let q = Random_Queue.simple_create city_count @@ Array.init city_count Fun.id in
    arg.solution <- Random_Queue.tot_empty q;
    arg.problem_size <- float_of_int @@ city_count - 1;
    arg.eval <- eval;
    arg.city_count <- city_count;
    let randomize = get_randomize_fun rnd_mode in
    let delta = get_delta_fun rnd_mode in
    let rec aux temp =
        if temp <= final_T then arg.solution else
        begin
            for _ = 1 to iter_count do
                random_choice();
                if Random.float 1. < exp (-. float_of_int (delta()) /. temp) then randomize()
            done;
            aux @@ temp *. cooling_ratio
        end
    in aux init_T


