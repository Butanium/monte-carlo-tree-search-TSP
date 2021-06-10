let city_count, cities = Readertsp.open_tsp "ulysses16";;
let eval = Basetsp.dists cities ;;
let s = TwoOpt.opt_best city_count eval (-1) (Array.init city_count Fun.id) in s;;