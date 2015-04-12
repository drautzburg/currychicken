f 0 = 0
f 1 = 1
f x 
          | x> 1 = 2
          | otherwise = -1

g 0 = 10
g 1 = 20
g x 
          | x> 1 = 30
          | otherwise = -40

h x = g (f x)