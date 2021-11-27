import matplotlib.pyplot as plt
import math
import statistics

import winsound


def show_int_list(
    fileName,
    sep=",",
    dir="C:/Users/Clement/Documents/prepa/tipe/ocaml-tsp/logs/score_logs",
    f=lambda x: x,
):
    file = open(dir + "/" + fileName, "r")
    values = list(map(int, (i for i in file.read().split(sep) if i)))
    plt.plot(f(values))
    winsound.MessageBeep()
    plt.show()


def show_tuple_float_list(
    fileName,
    sep1=";",
    sep2=",",
    dir="C:/Users/Clement/Documents/prepa/tipe/ocaml-tsp/logs",
    abs_index=0,
    ord_index=-1,
    f=lambda x, y: (x, y),
    max_values=float("infinity"),
    title="",
    x_label="playout count",
    y_label="playout score",
):
    file = open(dir + "/" + fileName, "r")
    lx = []
    ly = []
    values = file.read().split(sep1)
    for i in range(int(min(len(values), max_values))):
        i = values[i]
        if not i:
            continue
        v = [j for j in i.split(sep2) if j]
        lx.append(float(v[abs_index]))
        ly.append(float(v[ord_index]))
    plx, ply = f(lx, ly)
    plt.figure(title)  # set the window title
    plt.plot(plx, ply)
    plt.xlabel(x_label)
    plt.ylabel(y_label)
    plt.title(title)
    winsound.MessageBeep()
    plt.show()


def moving_average(n, ly):
    return [
        statistics.mean(ly[n * i : min(len(ly), (i + 1) * n)])
        for i in range(math.ceil(len(ly) / n))
    ]
