# Monte Carlo Tree Search for TSP

In this repo, I made different heuristic solutions for the TSP written in OCaml. My main goal is to study the effectiveness of the Monte Carlo Tree Search to solve the TSP.

## Available heuristics :

- [**2-opt**](https://en.wikipedia.org/wiki/2-opt) : classic 2-opt algorithm, which makes a locally optimized solution
  from any path.
- [**Monte-Carlo tree search**](https://en.wikipedia.org/wiki/Monte_Carlo_tree_search) or *MCTS*
- [**Simulated annealing**](http://rbanchs.com/documents/THFEL_PR15.pdf) or *SA*
- [**Genetic algorithm**](https://en.wikipedia.org/wiki/Genetic_algorithm) : maybe one day... For now, I only did it in
  [python](https://github.com/Butanium/Genetic_algorithm_for_TSP_python)
- **Greedy Random** : try to find a solution by generating bunch of random solutions (with or without heuristics). It as not meant to be good, it's just to compare with the MCTS performances

## Objectives :

My point with MCTS is to use the approach described
in [this paper](http://sasimi.jp/new/sasimi2016/files/archive/pdf/p352_R4-14.pdf) and try to see if adding some 2-opt in
playouts and reduce their size, can end up in a better result.

## How can I test it ?

If you don't want to use dune you can still run an old version of the project online - see toplevel release.

If you have dune, be sure that the Graphics library is installed in your opam switch
- Open and build the project using `dune build`. 
- Then chose an example from the `example` directory and run it using `dune exec ./examples/example_name.exe`

## Ressources

[TSPLIB](http://comopt.ifi.uni-heidelberg.de/software/TSPLIB95/tsp/) a library which contains different cities
configuration used in most of the papers related to TSP

## How can I ask my questions or look for help ?

You can contact me by email or by joining [my discord server](https://discord.com/invite/DWRJxA5yHB)