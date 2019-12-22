{-|
Module      : W
Description : Transform guitar movements to MIDI
Copyright   : (c) Martin Drautzburg 2019
License     : GPL-3
Maintainer  : Martin.Drautzburg@web.de
Stability   : experimental
Portability : POSIX

This module allows writing a guitar or bass score in terms of
instrument-specific actions. Actions incluse things like "put a finger
on the 3rd fret of the G-string" or "pluck the G-String".
-}

module Gear.Player.Guitar where

import Control.Monad.Trans.State.Lazy
import CommonFormatting
import Data.Ratio
import Data.Maybe
import qualified Data.Map.Lazy as M
import Control.Monad.Trans.Writer.Lazy
import Euterpea.Music (Pitch, Volume, PitchClass(..))
import qualified Euterpea.IO.MIDI.MEvent as EE
import qualified Euterpea.IO.MIDI.Play  as EP
import qualified Data.EventList.Absolute.TimeBody as Tb

import Gear.FrpTb 
import Gear.Midi.Track

type Time = Rational
type Dur  = Rational

-- | Pitch offset in cents for string bending and whammy bar (100 = 1 semitone)
type Cents = Int

-- | A string is identified by its Pitch
type GString = Pitch

------------------------------------------------------------
-- * States of Fret, String and Guitar

-- | The position of your left hand finger on a 'GString'. @Fret 0@ is open string.
data FretState = Fret Int | Damped 
  deriving (Eq, Show)

-- | The complete state of a string 
data StringState = StringState {
  fret   :: FretState,
  ring   :: (Time,Volume), -- ^ when the string was plucked. Indicates current ringing level
  bent   :: Cents
  } deriving (Eq, Show)


-- | The state of the Guitar is mostly the states of the strings
data GuitarState = GuitarState {
  strings :: M.Map GString StringState, 
  whammy  :: Cents,        -- ^ the position of the whammy bar (0 is neutral)
  gvol    :: Volume         -- ^ the position of the volums knob
  } deriving (Eq, Show)

-- | The pitches of the strings on a regular 6string guitar
guitarStrings :: [GString]
guitarStrings = [(E,2), (A,2), (D,3),(G,3),(B,3),(E,4)]

-- | The pitches of the string on a regular 4string bass
bassStrings :: [GString]
bassStrings = [(E,1), (A,1), (D,2),(G,2)]

-- | Initialize a GuitarState for the given GStrings
instrumentWith :: [GString] -> GuitarState
instrumentWith strs = let initialStrings :: [(GString, StringState)]
                          initialStrings = [(s,StringState Damped (0,0) 0)| s<-strs]
                      in GuitarState  (M.fromList initialStrings) 0 100

guitar :: GuitarState
guitar = instrumentWith guitarStrings

--------------------------------------------------------------------------------
-- * State updating TODO need beautification

type Lens sub a = a -> (sub, sub->a)

focus :: (Lens s' s) -> State s' a -> State s a
focus lens ms'= do
    s <- get
    let (s', set) = lens s
        (a, s'')  = runState ms' s'
    put (set s'')
    return a

fret2string :: Lens FretState StringState
fret2string (StringState f r b) = (f, \f' -> StringState f' r b)

onStrings :: Lens (M.Map GString StringState) GuitarState
onStrings (GuitarState s w v) = (s, \s' -> GuitarState s' w v)

adjustString :: GString -> (StringState -> StringState) -> M.Map GString StringState -> M.Map GString StringState 
adjustString = flip M.adjust

--------------------------------------------------------------------------------
-- * Player Actions

-- | What a guitarist can do to a string
data StringAct = DoFret FretState | DoBend Cents | DoPluck Volume
  deriving (Eq, Show)

-- | What a guitarist can do to the whole Guitar
data GuitarAct = DoString GString StringAct |
                 DoWhammy Cents |
                 DoVolume Volume
  deriving (Eq, Show)

                  
-- | A list of 'GuitarAct's with the time of occurrence
type Score a     = Events Time a
type GuitarScore = Score GuitarAct

--------------------------------------------------------------------------------
-- * Higher-level Actions

-- | Generate a 'GuitarScore' representing a strum across all Strings (lowest to highest)
strumDown :: [GString] -> Dur -> Volume -> Time -> GuitarScore
strumDown strs dt vol t  = let acts = DoString <$> strs <*> [DoPluck vol]
                               times = iterate (+ dt) t
                           in fromPairList $ ( zip .iterate (+ dt)) t acts

-- | Strum from highest to lowest string
strumUp = strumDown . reverse 

--------------------------------------------------------------------------------
-- * Rendering

-- Rendering is composed from functions of this type. TODO: Tempo

type Renderer sc st  = Score sc -> State st [Track]


-- | Return Tracks and the final state like runState)
runRenderer :: Renderer GuitarAct GuitarState -> GuitarScore -> GuitarState -> ([Track], GuitarState)
runRenderer rdr gScore = runState (rdr gScore)

---- | Return a single, merged Track
-- render score = let (tracks, s) = runRenderer score stdGuitar
--                in merge tracks



stillRinging :: Time -> StringState -> Bool
stillRinging t ss
  | tr > t     = error "ringing time"
  | (t-tr) < 1 = True -- TODO: volume
  | otherwise  = False
    where (tr, vr) = ring ss

  

renderFret :: FretState -> State StringState Track
renderFret fretAct = let setFret fs = focus fret2string (put fs >> return emptyTrack)
                     in
                       do
                         ss <- get
                         case (fret ss, ring ss, fretAct) of
                           (Damped, _, _) -> setFret fretAct





exE = [
  (1%4, DoString (E,2) $ DoFret (Fret 4)),
  (1%4, DoString (E,2) $ DoPluck 80),
  (1%4, DoString (E,2) $ DoFret (Fret 4))
  ]



exS = fromPairList exE :: GuitarScore





