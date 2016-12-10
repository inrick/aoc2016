import numpy as np

input = open('input.txt','r').readlines()[0].rstrip().split(', ')
directions = np.array([1 if x == 'R' else -1 for x in (x[0] for x in input)])
magnitudes = np.array([int(x[1:]) for x in input])
moves = np.array([[0,1],[1,0],[0,-1],[-1,0]])[np.mod(np.cumsum(directions), 4)]
print(sum(abs(np.dot(magnitudes, moves)))) # part 1
