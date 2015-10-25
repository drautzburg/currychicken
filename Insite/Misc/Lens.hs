{-|
Module      : Lens
Description : provides poor-man's Lens support
Copyright   : (c) Martin Drautzburg, 2015
License     : GPL-3
Maintainer  : Martin.Drautzburg@web.de
Stability   : experimental
Portability : POSIX

Implements 'focus' which allows stateful operations on a smaller part
of a state. There is an existing Lens package on hackage, but we need
a lot less here.

-}


module Misc.Lens where

import Control.Monad.State.Strict

-- | Helper for operations on sub-state
type Lens sub a = a -> (sub, sub->a)

-- | This is useful when you have an operation on 'System' but you
-- need an operation on 'SimState'.


focus :: (Lens s' s) -> State s' a -> State s a
focus lens ms'= do
    s <- get
    let (s', set) = lens s
        (a, s'')  = runState ms' s'
    put (set s'')
    return a

