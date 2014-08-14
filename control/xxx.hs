import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map as M
import Data.Monoid

type SkillSet = S.Set String 

------------------------------------------------------------
-- Request
------------------------------------------------------------
type Name  = String
type Count = Integer


data  Request  = Erq Name SkillSet | Qrq SkillSet Count | NoRequest



------------------------------------------------------------
-- Resource Pool
------------------------------------------------------------

type Eresources = M.Map Name SkillSet
type Qresources = M.Map SkillSet Count
type Pool = (Eresources, Qresources)

ers name skills = (name, S.fromList skills)
qrs skills count = (S.fromList skills, count)

examplePool :: Pool
examplePool = (
  M.fromList [
     ers "IRV1" ["runProg100", "runProg200"],
     ers "IRV2" ["runProg100", "runProg200"],
     ers "IRV3" ["runProg100", "runProg200", "runProg201"]
     ] 
  ,
  M.fromList [
    qrs ["pack"]            2,
    qrs ["unpack"]          3,
    qrs ["pack", "unpack"]  3,
    qrs ["drive", "unpack"] 3
    ] )




------------------------------------------------------------
-- Adding and removing Resources
------------------------------------------------------------


removeQuantified :: SkillSet -> Count -> Qresources -> Qresources
removeQuantified skills count qrs
  | M.member skills qrs = let (Just avail) = M.lookup skills qrs
                          in case (compare count avail) of
                            EQ -> M.delete skills qrs
                            GT -> error "cannot remove that many." 
                            LT -> M.insert skills (avail - count) qrs
  | otherwise           = error "Resource not in Pool"


addQuantified :: SkillSet -> Count -> Qresources -> Qresources
addQuantified skills count qrs 
  | M.member skills qrs = removeQuantified skills (-count) qrs
  | otherwise           = M.insert skills count qrs


removeEnumerated :: Name -> SkillSet -> Eresources -> Eresources
removeEnumerated name _  ers
  | M.member name ers = M.delete name ers
  | otherwise         = error "Cannot remove - Resource is not in Pool."

addEnumerated :: Name -> SkillSet -> Eresources -> Eresources
addEnumerated nm sk ers
  | M.member nm ers = error "Resource already in Pool."
  | otherwise       = M.insert nm sk ers


------------------------------------------------------------
-- Finding resources
------------------------------------------------------------

findResources1 :: Request -> Eresources -> (Request, Maybe Name)
findResources1  rq@(Erq name skills) ers = case (M.lookup name ers) of
  Nothing -> (rq, Nothing)
  Just r  -> (NoRequest, Just name)

findResources2  rq@(Qrq skills count qrs = 1
  where
    sortBy $ toList $ M.filterWithKey (\k v -> k `isSubsetOf` skills) qrs




------------------------------------------------------------
-- Event
------------------------------------------------------------

type Time = Integer
type Event a = (Time,a)

------------------------------------------------------------
-- Workorder
------------------------------------------------------------
type Task  = String
type From  = Time
type Until = Time
type Workorder = (Task, [(From, Until, Request)])



