import Data.List
import GHC.Exts
import Data.Function
import qualified Data.Map as M
import qualified Data.List.Ordered as O
import Data.List.Split
import Data.Maybe
import Debug.Trace
import System.Process
import Control.Monad.State
import Data.Graph.Inductive.Graph 
import qualified Data.Graph.Inductive.PatriciaTree as P
import Control.Arrow
import System.Info


ex1 :: P.Gr String String
ex1 = mkGraph [ (1,"one")
              , (3,"three")
              ]
              [ (1,3,"edge label1"),
                (1,3,"edge label2"),
                (1,3,"edge label2")
              ]
