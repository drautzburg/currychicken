import Temporal
f = (Temporal {td = -3, tc = []}) >>= \t1 ->
    (Temporal {td = -4, tc = [Chg {ct = -1, cv = -2}]}) >>= \t2 ->
    return (t1,t2)


g = fmap (\t2 -> return t2 ::Temporal Int) (Temporal {td = -4, tc = [Chg {ct = -1, cv = -2}]}) 
