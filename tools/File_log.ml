type debug_file = { file_name : string; file_path : string }

let create_dir_if_not_exist dir =
  let dirs = String.split_on_char '/' dir in
  let rec aux = function
    | [] -> [ "" ]
    | x :: xs ->
        "" :: List.map (fun s -> if s = "" then x else x ^ "/" ^ s) (aux xs)
  in
  let dirs = List.tl @@ aux dirs in
  List.fold_left
    (fun acc dir ->
      if not (Sys.file_exists dir && Sys.is_directory dir) then (
        Sys.mkdir dir 0;
        false)
      else acc)
    true dirs

let create_file ?(file_name = "log") ?(file_path = "logs") ?(extension = "csv")
    () =
  let _ = create_dir_if_not_exist file_path in
  let base_file_name = file_name in
  let file_name = ref @@ base_file_name ^ "." ^ extension in
  let i = ref 0 in
  while Sys.file_exists @@ file_path ^ "/" ^ !file_name do
    file_name := base_file_name ^ "_" ^ string_of_int !i ^ "." ^ extension;
    incr i
  done;
  { file_name = !file_name; file_path = file_path ^ "/" }

let create_log_dir dir_name =
  if create_dir_if_not_exist dir_name then
    let rec aux i =
      let name = Printf.sprintf "%s--%d" dir_name i in
      if create_dir_if_not_exist name then aux (i + 1) else name
    in
    aux 1
  else dir_name

let get_oc file = open_out @@ file.file_path ^ file.file_name

let log_string_endline ?(close = true) ?oc file data =
  let oc = Option.value oc ~default:(get_oc file) in
  Printf.fprintf oc "%s\n" data;
  if close then close_out oc;
  oc

let log_string ?(close = true) ?oc file data =
  let oc = Option.value oc ~default:(get_oc file) in
  Printf.fprintf oc "%s" data;
  if close then close_out oc;
  oc

let log_data ?(close = true) ?oc convert_data ?file (datas : 'a list) =
  if file = None && oc = None then
    raise
      (Invalid_argument "[log_data] you need to specify either a file or an out channel");
  let oc = Option.value oc ~default:(get_oc @@ Option.get file) in
  List.iter (fun x -> Printf.fprintf oc "%s" @@ convert_data x) datas;
  if close then close_out oc;
  oc
