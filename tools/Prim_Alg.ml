module IntSet = Set.Make(Int)
let prim_alg eval city_count =
    let init_visited = IntSet.singleton 0 in
    let init_visit =
        let rec aux1 i acc =
        if i < city_count then aux1 (i + 1) (IntSet.add i acc) else acc
        in aux1 1 IntSet.empty
    in
    let rec aux to_visit visited score =
        if not @@ IntSet.is_empty to_visit then (
            let added = ref (-1) in
            let mini = ref max_int in
            IntSet.iter (fun c1 ->
                IntSet.iter (fun c2 ->
                    let len = eval c1 c2 in
                    if len < !mini then (
                        mini := len;
                        added := c1
                    )) visited) to_visit;
            let new_to_visit = IntSet.remove !added to_visit
            in
            let new_visited = IntSet.add !added visited
            in
            aux new_to_visit new_visited (score +  !mini)
        ) else (
            score
        )
    in
    aux init_visit init_visited 0


