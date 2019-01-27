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
type DPitch = Int

-- | A string is identified by its Pitch
type GString = Pitch

-- * States of Fret, String and Guitar

-- | The position of your left hand finger on a 'GString'
data FretState = Fret Int | Open | Damped 
  deriving (Eq, Show)

-- | The complete state of a string 
data StringState = StringState {
  tuning :: GString, 
  fret   :: FretState,
  ringing:: (Time,Volume), -- ^ when the string was plucked. Indicates current ringing level
  bent   :: DPitch
  } deriving (Eq, Show)

-- | Initialize a StringState for a String of given Pitch
stringState :: GString -> StringState
stringState p = StringState p Damped (0,0) 0

-- | The state of the Guitar is mostly the states of the strings
data GuitarState = GuitarState {
  strings :: [StringState], -- ^ identifies the 'GString'
  whammy  :: DPitch,        -- ^ the position of the whammy bar (0 is neutral)
  gvol    :: Volume         -- ^ the position of the volums knob
  } deriving (Eq, Show)

-- | The pitches of the strings on a regular 6string guitar
stdGuitarStrings :: [GString]
stdGuitarStrings = [(E,2), (A,2), (D,3),(G,3),(B,3),(E,4)]

stdGuitar :: GuitarState
stdGuitar = GuitarState (map stringState stdGuitarStrings) 0 100

-- | The pitches of the string on a regular 4string bass
stdBassStrings :: [GString]
stdBassStrings = [(E,1), (A,1), (D,2),(G,2)]

stdBass :: GuitarState
stdBass = GuitarState (map stringState stdBassStrings) 0 100

-- * State updating TODO need beautification

-- type Lens a = GuitarState -> (a, a->GuitarState->GuitarState) TODO
-- getStringState :: GString -> Lens StringState
-- getStringState str gs = find ((== str) . tuning) strings gs

setStringStates :: (StringState -> StringState) -> GuitarState -> GuitarState
setStringStates fup gs = gs{strings = fmap fup (strings gs)}

onString str fup  ss 
  | tuning ss == str = fup ss
  | otherwise = ss

                            

-- -> GuitarState -> (Track, GuitarState)
-- | modify the state of a particular string
-- updStringState  :: Upd StringState -> Rnd GuitarState
-- updStringState = undefined
-- updStringState str fup gs = let tf s q
--                                   |tuning s == q = fup s
--                                   |otherwise     = s
--                             in traverse tf (strings gs) str

-- * Player Actions

-- | What a guitarist can do to a string
data StringAct = DoFret FretState | DoBend DPitch | DoPluck Volume
  deriving (Eq, Show)

-- | What a guitarist can do to the whole Guitar
data GuitarAct = DoString GString StringAct |
                 DoWhammy DPitch |
                 DoVolume Volume
  deriving (Eq, Show)

                  
-- | A list of 'GuitarAct's with the time of occurrence                  
type GuitarScore = Events Time GuitarAct

-- * Higher-level Actions

-- | Generate 'GuitarScore' representing a strum across all Strings (lowest to highest)
-- strumDown :: [GString] -> Dur -> Volume -> Time -> GuitarScore
-- strumDown strings dt vol t  = let acts = (fmap . DoString) (DoPluck vol) strings
--                                   times = iterate (+ dt) t
--                               in fromPairList $ zip times acts

-- | Strum from highest to lowest string
-- strumUp = strumDown . reverse 

-- * Rendering

-- Rendering is composed from function of this type. TODO: Tempo
type RenderF = Time -> GuitarAct -> GuitarState -> (Track, GuitarState)
type Renderer = (Time, GuitarAct) -> State GuitarState Track

toRenderer :: RenderF -> Renderer
toRenderer rf = let f (t,g) = state (\s -> rf t g s)
                in f


-- | Return Tracks and the final state like runState)
runRenderer :: GuitarScore -> GuitarState -> ([Track], GuitarState)
runRenderer = runState . mapM theRenderer . Tb.toPairList

-- | Return a single, merged Track
render score = let (tracks, s) = runRenderer score stdGuitar
               in merge tracks

theRenderer :: Renderer
theRenderer = let r t (DoString str (DoFret fs)) = setFret str fs
                  r t (DoString str (DoPluck v)) = undefined
                  r t  _                         = undefined
              in toRenderer r


setFret :: GString -> FretState -> GuitarState -> (Track, GuitarState)
setFret str fs gs = let set f' ss = ss{fret = f'}
              in (emptyTrack, (setStringStates . onString str . set) fs gs)


exE = [
  (1%4, DoString (E,2) $ DoFret (Fret 4)),
  (1%4, DoString (E,2) $ DoPluck 80),
  (1%4, DoString (E,2) $ DoFret (Fret 4))
  ]



exS = fromPairList exE :: GuitarScore




