type 'a t = {mutable available : 'a list; mutable create_fun : unit -> 'a;}

let create create_fun = {available = []; create_fun}

let set_create_fun manager f = manager.create_fun <- f 
let get manager = match manager.available with 
  | x :: xs -> manager.available <- xs; x
  | _ -> manager.create_fun()

let free manager element = 
  manager.available <- element :: manager.available;
