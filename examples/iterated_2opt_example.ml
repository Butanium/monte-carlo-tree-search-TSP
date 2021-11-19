let city_config = "a280";;
let city_count, cities = Readertsp.open_tsp city_config;;
let eval = Basetsp.dists cities;;
let path = TwoOpt.iter_two_opt 10 eval city_count TwoOpt.Random;;
Showtsp.show_solution cities path;;

let len = Basetsp.path_length eval path in
let best_len = Basetsp.best_path_length city_config eval in
    Printf.printf "\n%% of error : %.2f %%" (100. *.float_of_int(len - best_len) /. float_of_int best_len);;

let best_path = Readertsp.open_path city_config;;
open Graphics;;
open_graph "";;
set_text_size 10;;
Graphics.set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";;
draw_string "uwuwuwuwu";;
