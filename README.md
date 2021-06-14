# Travelling on ocaml salesman problem
Different heuristic solutions for the TSP written in ocaml.
My goal is to compare and combine differents solution in order to find an optimal one.
## Available heuristics : 
- [**2-opt**](https://en.wikipedia.org/wiki/2-opt) : classic 2-opt algorithm, which makes a locally optimized solution from any path.
- [**Monte-Carlo tree search**](https://en.wikipedia.org/wiki/Monte_Carlo_tree_search) aka MCTS 1 and 2 : (1) tries to advance in the tree until it built the whole path, (2) Keep the same root during the whole run and return the best path encounter during playouts
-  [**Genetic algorithm**](https://en.wikipedia.org/wiki/Genetic_algorithm) : not started
-  ???
## Objectives :
My point with MCTS is to use the approach described in [this paper](http://sasimi.jp/new/sasimi2016/files/archive/pdf/p352_R4-14.pdf) and try to see if adding some 2-opt in playouts can end up in better result.
## Ressources
[TSPLIB](http://comopt.ifi.uni-heidelberg.de/software/TSPLIB95/tsp/) a librairy which contains different cities configuration used in most of the papers related with tsp 
