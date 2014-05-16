2048-hs
==========

2048 Game and AI, written in Haskell

2048 Original:

Full alpha-beta pruning with the expectiminimax algorithm, which can currently run until depth 7 with no real issues. The issue is balancing between the expectiminimax algorithm (which can't have pruning) and the traditional minimax approach (which treats the probabilistic steps as an adversary). Right now it uses expectiminimax when the branching factor is suitably low. Current work is focused on making good heuristics (and also efficient heuristics).



2048-3D Game and AI:

It's pretty straightforward, nothing too amazing. You can use naive minimax with no pruning at a very low depth, because it turns out the game is fairly easy to solve. The current program will always win :)
