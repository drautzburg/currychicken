

foo (x:[]) = (x:)
foo (x:xs) = fmap (x:) (foo xs)

bar = foo [x*x | x <- [1,2,3]]
