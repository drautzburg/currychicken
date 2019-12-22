{-|
Module      : W
Description : Send Midi Output via Sound.ALSA.Sequencer
Copyright   : (c) Martin Drautzburg 2018
License     : GPL-3
Maintainer  : Martin.Drautzburg@web.de
Stability   : experimental
Portability : POSIX

This module is a wrapper around @Sound.ALSA.Sequencer@ of the
@alsa-seq@ package by Henning Thielemann. It is intended for sending
short sequences of Midi Events.

-}      
module Gear.Midi.Alsa where
import GHC.Word
import Control.Monad
import qualified Data.EnumSet as EnumSet
import Text.Printf (printf, )
import qualified Text.Pretty.Simple as P
import Data.Ratio

import Data.Foldable (traverse_)
import Control.Applicative
import Data.Semigroup (Max(..))
-- Sound.ALSA.Sequencer
import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Client.Info as ClientInfo
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Port.Info as PortInfo
import qualified Sound.ALSA.Sequencer.Event as Evt
import qualified Sound.ALSA.Sequencer.Queue as Queue
import qualified Sound.ALSA.Sequencer.Queue.Tempo as Tempo
import qualified Sound.ALSA.Sequencer.Time as Time
import qualified Sound.ALSA.Sequencer.Connect as Conn
import qualified Sound.ALSA.Sequencer as SndSeq

-- | Opaque wrapper around several alsa types.        
data Ifc = Ifc {
  name   :: String,
  h      :: SndSeq.T SndSeq.DuplexMode,
  client :: Client.T,
  port   :: Port.T,
  conn   :: Conn.T,
  queue  :: Queue.T
  }

-- * Opening and closing

-- | List the available ports on your system (like @aplaymidi
-- -l@). This is code is from @examples/list-ports.hs@ in the
-- @alsa-seq@ package. It creates output like this:
--
-- >>> listPorts
--  Port    Client name                      Port name                Caps
--   0:0    System                           Timer                    rRw 
--   0:1    System                           Announce                 rR  
--  14:0    Midi Through                     Midi Through Port-0      rRwW
--  16:0    ESS ES1938 (Solo-1)              ESS ES1938 (Solo-1) MIDI rRwW
--  17:0    OPL3 FM synth                    OPL3 FM Port               wW
--  20:0    M Audio Audiophile 24/96         M Audio Audiophile 24/96 rRwW
-- 128:0    FLUID Synth (qsynth)             Synth input port (qsynth   wW

listPorts :: IO ()
listPorts = do
  putStrLn " Port    Client name                      Port name                Caps"
  SndSeq.withDefault SndSeq.Block $ \h ->
    ClientInfo.queryLoop_ (h :: SndSeq.T SndSeq.OutputMode) $ \cinfo -> do
      client <- ClientInfo.getClient cinfo
      PortInfo.queryLoop_ h client $ \pinfo -> do
        join $ liftM5 (printf "%3d:%-3d  %-32.32s %-24.24s %s\n")
          (fmap (\(Client.Cons p) -> p) $ PortInfo.getClient pinfo)
          (fmap (\(Port.Cons p) -> p) $ PortInfo.getPort pinfo)
          (ClientInfo.getName cinfo)
          (PortInfo.getName pinfo)
          (do
             caps <- PortInfo.getCapability pinfo
             let disp (cap, char) =
                    if EnumSet.disjoint caps cap then ' ' else char
             return $ map disp $
               (Port.capRead, 'r') :
               (Port.capSubsRead, 'R') :
               (Port.capWrite, 'w') :
               (Port.capSubsWrite, 'W') :
               [])
                                          
-- | Creates an 'Ifc' to the given MIDI device (client). This is
-- typically done at the beginning of playback. With the ports from
-- the example above, you'll get
--
-- >>> open "20:0"    -- connects to 20:0    M Audio Audiophile 24/96
-- >>> open "M Audio" -- connects to 20:0    M Audio Audiophile 24/96
-- >>> open "M"       -- connects to 14:0    Midi Through
-- >>> open "20:1"    
-- *** Exception: AlsaException.Cons "connect_to" "Invalid argument" (Errno 22)
open :: String -> IO Ifc
open ifcName = do
    h <- SndSeq.openDefault SndSeq.Block :: IO (SndSeq.T SndSeq.DuplexMode)
    Client.setName h "Gear"             -- client name
    port <- Port.createSimple h ifcName
            (Port.caps [Port.capRead, Port.capSubsRead, Port.capWrite])
            (Port.types [Port.typeMidiGeneric, Port.typeApplication])
    client <- Client.getId h 
    addr <- Addr.parse h ifcName -- "ESS"          -- interface (soundcard) name
    conn <- Conn.createTo h port addr
    -- TODO: with multiple synths, one queue for all would suffice.
    queue <- Queue.alloc h
    setTempoDefault h queue
    Queue.control h queue Evt.QueueStart Nothing
    putStrLn ("opened Ifc " ++ ifcName)
    printTempo h queue
    -- Queue.control h queue (Evt.QueueTempo qTempo ) Nothing -- tempo


    return $ Ifc ifcName h client port conn queue

-- | Closes the 'Ifc'. This is typically done at the end of
-- playback. You can observe 'Gear' opening and closing on the @Alsa@
-- tab of qjackctl.
close :: Ifc -> IO ()                                             
close ifc = 
    let (Ifc name h client port conn queue) = ifc
    in do
        Evt.drainOutput h
        pending <- Evt.outputPending h 
        putStrLn $ "Events pending = " ++ (show pending)
        putStrLn ("closed Ifc " ++ name)
        Queue.control h  queue Evt.QueueStop Nothing
        Port.delete h port
        putStrLn "..."


-- * Sending data

-- | Events with a 'Tick' timestamp
type Evt = (Tick, Evt.Data)

-- | Return the 'Tick' of the last (latest) event
tLast :: [Evt] -> Tick
tLast = maximum . map fst 

-- | Send a single Event, return its Tick
sendEvt :: Ifc -> Evt -> IO Tick
sendEvt ifc evt = do
        let (Ifc name h client port conn queue) = ifc
            toEvt (t,e) = (Evt.forConnection conn e) {
                            Evt.queue = queue,
                            Evt.time = Time.consAbs $ Time.Tick t
                        }
          in do
                Evt.output h (toEvt evt)
                return (fst evt)
                
-- | Send a a list of Events, drain output, return latest Tick
sendEvts :: Ifc -> [Evt] -> IO Tick
sendEvts ifc evts = let sendEvt' e = Max <$> sendEvt ifc e
                    in
                      do
                        tMax <- foldMap sendEvt' evts
                        Evt.drainOutput (h ifc)
                        -- Evt.outputPending (h ifc)
                        return (getMax tMax)

-- | play Events over the interface with the given name. Block until done
play ifcName evts = do
  ifc <- open ifcName
  tlast <- sendEvts ifc evts
  blockUntil ifc tlast
  close ifc
  
  

blockUntil :: Ifc -> Tick -> IO()                                     
blockUntil ifc t = let (Ifc name h client port conn queue) = ifc
                       me = Addr.Cons client port
                       sendEcho = do
                           putStr $ "blocking until " ++ (show t) ++ " ... "
                           Evt.output h (Evt.forConnection conn (Evt.CustomEv Evt.Echo $ Evt.Custom 0 0 0)){
                                      Evt.queue = queue,
                                      Evt.dest = me,
                                      Evt.time = Time.consAbs $ Time.Tick t
                                  }
                           Evt.drainOutput h
                       isEcho evt = case Evt.body evt of
                                        Evt.CustomEv Evt.Echo _ -> Evt.source evt == me
                                        _ -> False
                       waitForEcho = do
                           event <- Evt.input h                                
                           when (not $ isEcho event) waitForEcho 
                           putStrLn $ " done t=" ++ (show $ Evt.time event)
                  in sendEcho >> waitForEcho
           
-- * Time and Tempo

-- | There are two parameters to define the actual tempo, PPQ (pulse per
-- quarter note) and MIDI tempo. The former defines the base resolution
-- of the ticks, while the latter defines the beat tempo in
-- microseconds (µsec/beat).
--
-- See <http://mcs.une.edu.au/doc/alsa-lib-devel/doxygen/html/seq.html>
--
-- This function sets PPQ='ppq' and tempo=500000 (120 BPM). The tempo
-- can be changed later by sending an Event.


setTempoDefault h queue = do
    qt <- Tempo.get h queue
    Tempo.setPPQ qt defaultPpq
    Tempo.setTempo qt 500000
    Tempo.set h queue qt

-- | One qn has 'ppq' 'Tick's, one Tick is as long as 1/ppq of a qn (quarter-note)
type Tick = Word32

-- | Time as a fraction of a whole note (so @1%4@ is a qn)
type Wholes = Ratio Int

-- | Pulses per quarter note. We use a higher resolution of 4*96
-- instead of the standard 96 PPQ.
defaultPpq = 4*96 :: Int

-- | Convert 'Wholes'  to 'Tick's. 
fromWholes :: Wholes -> Tick
fromWholes qn = fromIntegral $ numerator qn * defaultPpq * 4 `div` denominator qn

-- | Print ppq and tempo for debugging             
printTempo h queue =
        let xtPrint lbl unit = putStrLn .   (lbl ++) . (++ " [" ++ unit ++ "]") . show
            tPrint lbl units = (\x->printf "%-8s=%7d [%s]\n" lbl x units) 
        in do
            qt <- Tempo.get h queue
            Tempo.getPPQ qt   >>= tPrint "PPQ" "ppq"
            Tempo.getTempo qt >>= tPrint "tempo" "µsec/beat"


-- * Example song


-- | Pitch as 8bit number
type Pitch = Word8

-- | Velocity as 8bit number
type Vel = Word8

-- | Events of an example Song
exampleEvts :: [Evt]
exampleEvts = let 
                noteEv p v      = Evt.simpleNote (Evt.Channel 0) (Evt.Pitch p) (Evt.Velocity v) :: Evt.Note
                noteOn, noteOff :: Pitch -> Vel -> Evt.Data 
                noteOn  p v   = Evt.NoteEv Evt.NoteOn  $ noteEv p v :: Evt.Data
                noteOff p v   = Evt.NoteEv Evt.NoteOff $ noteEv p v :: Evt.Data
                wholeNote :: Wholes -> Pitch -> Vel -> [Evt]
                wholeNote t p v = [
                  (fromWholes t, noteOn p v),
                  (fromWholes (t+1 ), noteOff p v)
                  ]

                arp :: Wholes -> Wholes -> [Pitch] -> [Evt]
                arp t dt keys = join $ zipWith3 wholeNote [t, t+dt .. ] keys (cycle [35,30,33,30])
              in
                  arp  0 (1%4) [60,64,67,71]  ++ -- Cj7
                  arp  1 (1%4) [60,64,67,71]  ++
                  arp  2 (1%4) [60,64,67,69]  ++ -- Am6
                  arp  3 (1%4) [60,64,67,69]  ++
                  arp  4 (1%4) [60,62,65,71]  ++ -- Dm7
                  arp  5 (1%4) [60,62,65,69]  ++
                  arp  6 (1%4) [59,62,65,69]  ++ -- G7
                  arp  7 (1%4) [59,62,65,67]  ++
                  arp  8 (1%32) [60,62,64,67]

-- | Play the 'exampleEvts' via a client named @ESS@. 

examplePlay :: IO ()
examplePlay = play "ESS" exampleEvts


          
{-

handleException :: IO () -> IO ()
handleException act =
   act
   `AlsaExc.catch` \e ->
      putStrLn $ "alsa_exception caught: " ++ AlsaExc.show e
               
------handleExceptionCont :: ContT () IO () -> IO ()
handleExceptionCont = handleException . runContUnit

runContUnit :: (Monad m) => ContT a m a -> m a
runContUnit cont = runContT cont return
-}
         
