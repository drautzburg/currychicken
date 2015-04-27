
type Row a = (a, Double)
data Table a b = Tbl [(a, b)]

t1 = Tbl[(1,1.0),(2,2.0)] :: Table Int Double

extend :: Table a Double -> (a -> Table b Double) -> Table a (Table b Double)
extend = undefined

