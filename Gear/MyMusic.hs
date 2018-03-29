import Debug.Trace


data Stanza a = Par [Stanza a] |
                Seq [Stanza a] |
                Lit a
    deriving (Eq, Show)

lst (Seq as) = as
lst (Par as) = as

cns (Seq _) = Seq
cns (Par _) = Par


line,chord :: [a] -> Stanza a
line  = Seq . (map Lit)
chord = Par . (map Lit)

l = line [1..4]
c = chord [1..4]

------------------------------------------------------------    
instance Foldable Stanza where
------------------------------------------------------------
    foldr f b s = let foo :: (a->b->b) -> Stanza a -> b -> b
                      foo fx sx bx = foldr fx bx sx
                      bar = undefined :: ()
                  in case s of
                         Lit a  -> f a b
                         _      -> foldr (foo f) b (lst s)
------------------------------------------------------------    
instance Functor Stanza where
------------------------------------------------------------
    fmap f s = case s of
        Lit a  -> Lit (f a)
        _      -> cns s $ map (fmap f) (lst s)


------------------------------------------------------------
instance Applicative Stanza where
------------------------------------------------------------
    pure = Lit
    sf <*> sa = undefined


------------------------------------------------------------
instance Monad Stanza where
------------------------------------------------------------
    return        = Lit
    sa >>= f      = join $ fmap f sa

join :: Stanza (Stanza a) -> Stanza a
join (Lit sa) = sa
join (Par sas) = sas
--join (Seq [sa]) = sa
