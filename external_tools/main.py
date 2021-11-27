"""after running a monte carlo tree search, some log files will appear in logs/*, 
    you can plot them here"""

from show_data import *

sim = "att48-1800s-Random-Min_spanning_tree-18128445_playouts"
sim =  "Greedy_Roulette-1800_s-71380653_tries"
best_score_dir = "best_score_logs/best_scores" #-
all_score_dir = "score_logs/all_scores"

"""show the evolution of the search's best score over playouts"""
show_tuple_float_list(
    best_score_dir + sim,
    title="best score over playouts",
    y_label="best score",
)


"""show the evolution of the search's best score over time"""
show_tuple_float_list(
    best_score_dir + sim,
    abs_index=1,
    title="best score over playouts",
    y_label="best score",
    x_label="time (s)",
)


"""show playout score over time"""
show_tuple_float_list(
    all_score_dir + sim,
    x_label="time (s)",
    title="playout score over time",
)


"""show playout scores"""
show_tuple_float_list(
    all_score_dir + sim,
    f=lambda _, ly: (list(range(len(ly))), ly),
    title="playout score ever playouts"
)


"""show playout score over time but with a moving average filter"""
show_tuple_float_list(
    "score_logs/all_scores-" + sim,
    f=lambda lx, ly: (moving_average(10000, lx), moving_average(10000, ly)),
    x_label="average time for 10,000 playouts",
    y_label="average score for 10,000 playout",
    title="playout score over time ",
)

"""show playout scores with a moving average filter"""
show_tuple_float_list(
    "score_logs/all_scores-" + sim,
    f=lambda _, ly: (
        moving_average(10000, list(range(len(ly)))),
        moving_average(10000, ly),
    ),
    y_label="average score for 10,000 playout",
    x_label = ""
)
