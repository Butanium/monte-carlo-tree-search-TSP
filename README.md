# Travelling on ocaml salesman problem

Different heuristic solutions for the TSP written in ocaml. My main goal is to study the effectiveness of the Monte
Carlo Tree Search to solve the TSP.

## Available heuristics :

- [**2-opt**](https://en.wikipedia.org/wiki/2-opt) : classic 2-opt algorithm, which makes a locally optimized solution
  from any path.
- [**Monte-Carlo tree search**](https://en.wikipedia.org/wiki/Monte_Carlo_tree_search)
- [**Simulated annealing**](http://rbanchs.com/documents/THFEL_PR15.pdf)
- [**Genetic algorithm**](https://en.wikipedia.org/wiki/Genetic_algorithm) : maybe one day... For now, I only did it in
  [python](https://github.com/Butanium/Genetic_algorithm_for_TSP_python)

## Objectives :

My point with MCTS is to use the approach described
in [this paper](http://sasimi.jp/new/sasimi2016/files/archive/pdf/p352_R4-14.pdf) and try to see if adding some 2-opt in
playouts and reduce their size, can end up in a better result.

## How can I test it ?

Open it and build it with dune. Try to run the files in examples as executable (Todo : setup executable)

## Ressources

[TSPLIB](http://comopt.ifi.uni-heidelberg.de/software/TSPLIB95/tsp/) a library which contains different cities
configuration used in most of the papers related to TSP
