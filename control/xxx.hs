import Control.Arrow

condAppend :: Bool -> String -> [String] -> [String]
condAppend c suff pref = if c then pref ++ [suff]
                         else pref

res = condAppend True "goodby" 
      $ condAppend False "wait a minute" 
      $ condAppend True "hello" ["Hi"]


res''= (
       condAppend True "hello"
       >>>
       condAppend False "wait a minute"
       >>>
       condAppend True "goodbye"
      ) ["Hi"]

res'= (
       condAppend True "hello"
       >>>
       condAppend False "wait a minute"
       >>>
       condAppend True "goodbye"
      ) ["Hi"]
        where
            f >>> g = g . f