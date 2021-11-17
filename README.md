# Travelling on ocaml salesman problem
Different heuristic solutions for the TSP written in ocaml.
My goal is to compare and combine different solution in order to find an optimal one.
## Available heuristics : 
- [**2-opt**](https://en.wikipedia.org/wiki/2-opt) : classic 2-opt algorithm, which makes a locally optimized solution from any path.
- [**Monte-Carlo tree search**](https://en.wikipedia.org/wiki/Monte_Carlo_tree_search) 
-  [**Simulated annealing**](http://rbanchs.com/documents/THFEL_PR15.pdf)
-  [**Genetic algorithm**](https://en.wikipedia.org/wiki/Genetic_algorithm) : coming soon ?
## Objectives :
My point with MCTS is to use the approach described in [this paper](http://sasimi.jp/new/sasimi2016/files/archive/pdf/p352_R4-14.pdf) and try to see if adding some 2-opt in playouts can end up in a better result.
## How can I test it ?
Just open [Main.ml](https://github.com/Butanium/monte-carlo-tree-search-and-TSP/blob/master/modules/Main.ml) in your ocaml interpreter and then use one of the example provided in [this directory](https://github.com/Butanium/monte-carlo-tree-search-and-TSP/blob/master/examples)
## Ressources
[TSPLIB](http://comopt.ifi.uni-heidelberg.de/software/TSPLIB95/tsp/) a library which contains different cities configuration used in most of the papers related to TSP
