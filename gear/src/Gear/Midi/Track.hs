{-|
Module      : W
Description : Functions to create and play Tracks
Copyright   : (c) Martin Drautzburg 2018
License     : GPL-3
Maintainer  : Martin.Drautzburg@web.de
Stability   : experimental
Portability : POSIX

This module facilitates

* writing Midi events to a data structure named 'Track'. This data
  structure is defined in __Sound.MIDI.File__ which is part of Henning
  Thielemann's __midi__ package.

* Saving and restoring Tracks. Only Midi files with a Division of
  /Ticks/ are supported (no /SMPTE/)

* sending Track data to a Midi device, i.e. playing Tracks though
  Alsa.

TODO

* Currently the channel is set before the time. Might be better the
  other way round.

* We might need more versatile operations to append tracks (see
  'initInstrument'). TimeBody probably has it all.

-}      
module Gear.Midi.Track where
import Data.Ratio

-- Sound.ALSA.Sequencer
import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Scl
import qualified Sound.ALSA.Sequencer.Client.Info as Sci
import qualified Sound.ALSA.Sequencer.Port as Sport
import qualified Sound.ALSA.Sequencer.Port.Info as Sporti
import qualified Sound.ALSA.Sequencer.Event as Sevt
import qualified Sound.ALSA.Sequencer.Queue as Squeue
import qualified Sound.ALSA.Sequencer.Queue.Tempo as SqTempo
import qualified Sound.ALSA.Sequencer.Time as STime
import qualified Sound.ALSA.Sequencer.Connect as Sconn
import qualified Sound.ALSA.Sequencer as S

-- Sound MIDI:
--import qualified Sound.MIDI.Message.Channel.Voice as Mvoice
--import qualified Sound.MIDI.Message.Channel as Mchan
import qualified Sound.MIDI.File as F
import qualified Sound.MIDI.File.Event as Fevt
import qualified Sound.MIDI.File.Save as Fsav
import qualified Sound.MIDI.File.Load as Flod

import qualified Sound.MIDI.Message.Channel as Mchan
import qualified Sound.MIDI.Message.Channel.Voice as McVoice
import qualified Sound.MIDI.Message.Channel.Mode as McMode
import qualified Sound.MIDI.Controller as Ctrl

import qualified Data.EventList.Relative.TimeBody as Tb

------------------------------------------------------------
-- * Type aliases

-- | Time in Ticks
type Tick = Integer

-- | Midi channel
type Ch = Int

-- | Velocity
type Vel = Int

-- | Note number
type Note = Int

-- | Controller number
type CNum = Int

-- | Controller value
type CVal = McVoice.ControllerValue

-- | Program number
type Prog = Int

-- | Tempo in pulses per quarter note
type PPQ = Int

-- | Sound.MIDI.File.Event.T (Event body)
type Evtb = Fevt.T

type Track = F.Track
------------------------------------------------------------
-- * Creating Voice events

-- | Convert a Message.Channel.Voice.T into a 'Evtb' by adding the channel
vChan :: Ch -> McVoice.T -> Evtb
vChan ch = Fevt.MIDIEvent . Mchan.Cons (Mchan.toChannel ch) . Mchan.Voice

-- insertEvt :: Tick -> Evtb -> Track -> Track


-- | create a noteOn 'Evtb' from channel, note-number and velocity
noteOn :: Ch -> Note -> Vel -> Evtb
noteOn ch n = vChan ch . McVoice.NoteOn (McVoice.toPitch n) . McVoice.toVelocity

-- | create a noteOff 'Evtb' from channel and note-number
noteOff :: Ch -> Note -> Evtb
noteOff ch n = vChan ch . McVoice.NoteOff (McVoice.toPitch n) $ McVoice.toVelocity 0

-- | create a programChange 'Evtb' 
programChange :: Ch -> Prog -> Evtb
programChange ch  = vChan ch . McVoice.ProgramChange . McVoice.toProgram

bankSelect :: Ch -> Int -> Evtb
bankSelect ch  = vChan ch . McVoice.Control Ctrl.bankSelect

-- | create a controller 'Evtb' from cannel, controller and controller value
controller :: Ch -> CNum -> CVal -> Evtb
controller ch cNum = vChan ch . McVoice.Control (McVoice.toController cNum) 

-- | create a mainVolume 'Evtb' from cannel and volume
mainVolume :: Ch -> CVal -> Evtb
mainVolume ch  = vChan ch . McVoice.Control Ctrl.volume 

-- | pitch bend. A value of 8192 is the neutral position. 
pitchBend :: Ch -> McVoice.PitchBendRange -> Evtb
pitchBend ch = vChan ch . McVoice.PitchBend

-- | bend in cents. A value of 0 is the neutral position. Assumes the
-- full range is +/- 12 semitones.
pitchBendCents :: Ch -> Int -> Evtb
pitchBendCents ch cents = let val = 8192 + cents * quot 8192 1200
                          in pitchBend ch val
------------------------------------------------------------
-- * Creating Mode events

-- | Convert a Message.Channel.Mode.T into a 'Evtb' by adding the channel
mChan :: Ch -> McMode.T -> Evtb
mChan ch = Fevt.MIDIEvent . Mchan.Cons (Mchan.toChannel ch) . Mchan.Mode

allNotesOff,resetAllControllers,allSoundOff :: Ch -> Evtb

allNotesOff ch = mChan ch McMode.AllNotesOff

resetAllControllers ch = mChan ch McMode.ResetAllControllers

allSoundOff ch = mChan ch McMode.AllSoundOff


-- 
-- 
------------------------------------------------------------
-- * Adding events to a Track

emptyTrack :: Track
emptyTrack = Tb.empty

-- | insert a single event to a Track
insertEvt :: Tick -> Evtb -> Track -> Track
insertEvt = Tb.insert . Fevt.toElapsedTime

-- | insert a whole lot of events into a Track
insertEvts :: Foldable t => Track -> t (Tick, Evtb) -> Track
insertEvts = foldr (uncurry insertEvt)

-- | insert a whole lot of events into an empty Track
fromList :: Foldable t => t (Tick, Evtb) -> Track
fromList = insertEvts emptyTrack

-- | delay second track by duration of first track and append
append :: Track -> Track -> Track
append = Tb.append

merge :: [Track] -> Track
merge = F.mergeTracks F.Mixed
------------------------------------------------------------
-- * Saving and restoring Tracks.


-- | Add tempo (ppq) to a list of tracks and save them
saveTracks :: FilePath -> PPQ -> [Track] -> IO()
saveTracks filename ppq = Fsav.toFile filename . (F.Cons F.Mixed . F.Ticks . F.toTempo) ppq

-- | Load a file and return the Tracks and the tempo
loadTracks :: FilePath -> IO ([Track],PPQ)
loadTracks filename = do
  (F.Cons typ (F.Ticks ppq) tracks) <- Flod.fromFile filename
  return (tracks, F.fromTempo ppq)

------------------------------------------------------------  
-- * Example

-- | Reset and set pitch bend range to 12 
initInstrument :: Ch -> Track 
initInstrument ch = fromList [
  -- reset
  (0, resetAllControllers ch),
  (0, allNotesOff ch),
  (0, allSoundOff ch),
  -- set pitch bend to neutral
  (1, pitchBend 1 8192),
  -- set pitch bend range to 12
  (2, controller ch 100 0),
  (3, controller ch 101 0),
  (4, controller ch 6 12)
  ]



-- | A C-minor chord with pitch bend
exampleTrack :: Track 
exampleTrack = initInstrument 1 
  `append` fromList [
  (0,  noteOn 1 60 80),
  (50,  noteOn 1 63 80),
  (100, noteOn 1 67 80), 
  (150, noteOn 1 72 80),
  (200, pitchBendCents 1 100),
  (250, pitchBendCents 1 200),
  (300, pitchBendCents 1 (-100)),
  (350, pitchBendCents 1 0),
  (400, noteOn 1 48 90),
  (450, allNotesOff 1)
  ] 

-- | Write 'exampleTrack' to xxx.mid
exampleFile = saveTracks "xxx.mid" 60 [exampleTrack]
