"""after running a monte carlo tree search, some log files will appear in logs/*, 
    you can plot them here"""

from show_data import *

sim = "att48-1800s-Roulette-Min_spanning_tree-25510560_playouts"

"""show the evolution of the search's best score"""
show_tuple_int_list("best_scores-" + sim)

"""show all scores"""
show_int_list("all_scores-" + sim)

"""show all scores but with a moving average filter"""
show_int_list("all_scores-" + sim, f=lambda l: moving_average(10000, l))
