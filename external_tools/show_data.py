import matplotlib.pyplot as plt
import math
import statistics


def show_int_list(
    fileName,
    sep=",",
    dir="C:/Users/Clement/Documents/prepa/tipe/ocaml-tsp/logs",
    f=lambda x: x,
):
    file = open(dir + "/" + fileName, "r")
    values = list(map(int, (i for i in file.read().split(sep) if i)))
    plt.plot(f(values))
    plt.show()


def show_tuple_int_list(
    fileName,
    sep1=";",
    sep2=",",
    dir="C:/Users/Clement/Documents/prepa/tipe/ocaml-tsp/logs",
):
    file = open(dir + "/" + fileName, "r")
    lx = []
    ly = []
    for i in file.read().split(sep1):
        if not i:
            continue
        x, y = i.split(sep2)
        lx.append(int(x))
        ly.append(int(y))
    print(lx[-1])
    plt.plot(lx, ly)
    plt.show()


def moving_average(n, l):
    return [
        statistics.mean(l[n * i : min(len(l), (i + 1) * n)])
        for i in range(math.ceil(len(l) / n))
    ]

#show_tuple_int_list("best_scores-att48-1200s-Roulette-Standard_deviation-15270762_playouts")

# show_int_list("all_scores-att48-1200s-Roulette-Standard_deviation-15270762_playouts")

show_int_list("all_scores-att48-1200s-Roulette-Standard_deviation-15270762_playouts",
f=lambda l: moving_average(10000, l))
