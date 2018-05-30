-- import Common
import GHC.Word
import Debug.Trace
import Sound.MIDI.ALSA
import Data.List
import Data.Function
import Control.Monad
import Control.Concurrent

-- Sound.ALSA.Sequencer
import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Port.Info as PortInfo
import qualified Sound.ALSA.Sequencer.Event as Evt
import qualified Sound.ALSA.Sequencer.Queue as Queue
import qualified Sound.ALSA.Sequencer.Time as Time
import qualified Sound.ALSA.Sequencer.Connect as Conn
-- import qualified Sound.ALSA.Sequencer.Time as Time
import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Sound.ALSA.Exception as AlsaExc
-- Sound MIDI:
--import qualified Sound.MIDI.Message.Channel.Voice as Mvoice
--import qualified Sound.MIDI.Message.Channel as Mchan
--import qualified Sound.MIDI.ALSA as Midi

import Control.Monad.Trans.Cont (ContT(ContT), runContT,)
import Control.Monad.IO.Class (liftIO, )
import Control.Monad (zipWithM_, )

-- import System.Environment (getArgs, )




handleException :: IO () -> IO ()
handleException act =
   act
   `AlsaExc.catch` \e ->
      putStrLn $ "alsa_exception caught: " ++ AlsaExc.show e
               
------handleExceptionCont :: ContT () IO () -> IO ()
handleExceptionCont = handleException . runContUnit

runContUnit :: (Monad m) => ContT a m a -> m a
runContUnit cont = runContT cont return


{-
-- Tempo --

There are two parameters to define the actual tempo, PPQ (pulse per
quarter note) and MIDI tempo. The former defines the base resolution
of the ticks, while the latter defines the beat tempo in
microseconds. As a default, 96 PPQ and 120 BPM are used,
respectively. That is, the tempo is set to 500000 (= 60 * 1000000 /
120). Note that PPQ cannot be changed while the queue is running. It
must be set before the queue is started.
-}

bpm = 120
ppq = 96 
qTempo = 60 * 10^6 * bpm
pq = (ppq *) -- convert quarter notes to pulses


type Time = Word32

-- * Output


sendEcho' :: SndSeq.AllowOutput mode =>
            SndSeq.T mode -> Client.T -> Port.T ->Conn.T -> Queue.T
         ->  Time -> IO ()
sendEcho' h client port conn queue  t =
        let me = Addr.Cons client port 
        in do Evt.output h (Evt.forConnection conn (Evt.CustomEv Evt.Echo $ Evt.Custom 0 0 0)){
                       Evt.queue = queue,
                       Evt.dest = me,
                       Evt.time = Time.consAbs $ Time.Tick t
                   }
              putStrLn $ "Sent echo at t=" ++ (show t)

waitForEcho ::SndSeq.AllowInput mode =>
    SndSeq.T mode -> Client.T -> Port.T -> IO ()
waitForEcho h client port = do
        event <- Evt.input h                                
        when (notMyEcho event) $ waitForEcho h client port
        putStrLn $ "Received echo at t=" ++ (show $ Evt.time event)
        where
            me = Addr.Cons client port
            notMyEcho evt = case Evt.body evt of
                                Evt.CustomEv Evt.Echo _ -> Evt.source evt /= me
                                _ -> True
        
output :: SndSeq.AllowOutput mode =>
          SndSeq.T mode -> Client.T -> Port.T -> Conn.T -> Queue.T 
       -> [(Time, Evt.Data)]-> IO ()
output h client port conn queue = output_t h client port conn queue 0 5000 

output_t :: SndSeq.AllowOutput mode =>
            SndSeq.T mode -> Client.T -> Port.T -> Conn.T -> Queue.T ->
            Time -> Time -> [(Time, Evt.Data)] -> IO()
    
output_t   h client port conn queue tBeg dt song =
    let tEnd = tBeg + dt
        evt (t,e)    = (Evt.forConnection conn e) {
                           Evt.queue = queue,
                           Evt.time = Time.consAbs $ Time.Tick t
                       } :: Evt.T
        (now, later) = break ((> tEnd) . fst) song
    in do
        putStrLn $ "playing t=" ++ show tBeg ++ " .. " ++ show tEnd ++ ", " ++ show (length later) ++ " events left"
        mapM_ (Evt.output h . evt) now
        if null later 
        then sendEcho' h client port conn queue (tEndSong now +1)
        else output_t  h client port conn queue tEnd dt later
        -- Note that this does not work
        --    when (not (null later)) (output_t  h client port conn queue tEnd dt later)
        --    sendEcho' h client port conn queue (tEnd+1)
             
        -- We will send an echo with each return from recursion
        -- (i.e. in reverse order), the last echo will be pretty
        -- early. The main program will pick this up and close the
        -- port. The song may still play to the end, because
        -- everything is already in an ALSA queue.

         

-- * Song data

type Pitch = Word8
type Vel = Word8
song' :: [(Time, Evt.Data)]
song' = let qt dt ctor p v t = (pq  (t+dt), ctor p v) :: (Time, Evt.Data)
            note p v      = Evt.simpleNote (Evt.Channel 0) (Evt.Pitch p) (Evt.Velocity v) :: Evt.Note
            noteOn  p v   = Evt.NoteEv Evt.NoteOn  $ note p v :: Evt.Data
            noteOff p v   = Evt.NoteEv Evt.NoteOff $ note p v :: Evt.Data
            t = [0,4 .. 12] :: [Time]
            f = [
                       qt 0 noteOn  60 42,
                       qt 5 noteOff 60 42,
                       qt 1 noteOn  64 30,
                       qt 6 noteOff 64 30,
                       qt 2 noteOn  67 40,
                       qt 7 noteOff 67 40,
                       qt 3 noteOn  71 40,
                       qt 8 noteOff 71 40
                  ] :: [Time -> (Time, Evt.Data)]
        in flip ($) <$> t  <*> f

tEndSong :: [(Time, Evt.Data)] -> Time
tEndSong = maximum . map fst 

-- * Real

data Ifc = Ifc {
            h      ::SndSeq.T SndSeq.DuplexMode,
            client :: Client.T,
            port   :: Port.T,
            conn   :: Conn.T,
            queue  :: Queue.T
            }


createOutPort :: SndSeq.T mode -> String -> IO Port.T
createOutPort h name = Port.createSimple h name
            (Port.caps [Port.capRead, Port.capSubsRead, Port.capWrite])
            (Port.types [Port.typeMidiGeneric, Port.typeApplication])

open :: String -> IO Ifc
open name = do
    h <- SndSeq.openDefault SndSeq.Block :: IO (SndSeq.T SndSeq.DuplexMode)
    Client.setName h "Gear"             -- client name
    port <- Port.createSimple h name
            (Port.caps [Port.capRead, Port.capSubsRead, Port.capWrite])
            (Port.types [Port.typeMidiGeneric, Port.typeApplication])
    client <- Client.getId h 
    addr <- Addr.parse h name -- "ESS"          -- interface (soundcard) name
    conn <- Conn.createTo h port addr
    queue <- Queue.alloc h
    Queue.control h queue Evt.QueueStart Nothing
    Queue.control h queue (Evt.QueueTempo (Evt.Tempo qTempo )) Nothing -- tempo
    return $ Ifc h client port conn queue

close :: Ifc -> IO ()                                             
close ifc = 
    let (Ifc h client port conn queue) = ifc
    in do
        Evt.drainOutput h
        Evt.outputPending h >>= (putStrLn . show)
        threadDelay $ 3*10^6
--        waitForEcho h client port
        putStrLn "done"                       -- cleanup
        Queue.control h  queue Evt.QueueStop Nothing
        Port.delete h port

            
send :: [(Time, Evt.Data)] -> Ifc -> IO Ifc
send evts ifc = do
        let (Ifc h client port conn queue) = ifc
            evt (t,e) = (Evt.forConnection conn e) {
                            Evt.queue = queue,
                            Evt.time = Time.consAbs $ Time.Tick t
                        }
         
        mapM_ (Evt.output h . evt) evts
        Evt.drainOutput h 
            -- Evt.outputPending h
        return ifc


main :: IO ()
main = handleExceptionCont $ liftIO $
       do
               -- setup
               h <- SndSeq.openDefault SndSeq.Block :: IO (SndSeq.T SndSeq.DuplexMode)
               Client.setName h "Gear"             -- client name
               port <- createOutPort h "out"       -- output port name

               client <- Client.getId h 
               addr <- Addr.parse h "FLUID" -- "ESS"          -- interface (soundcard) name
               conn <- Conn.createTo h port addr
               queue <- Queue.alloc h
               Queue.control h queue Evt.QueueStart Nothing
               Queue.control h queue (Evt.QueueTempo (Evt.Tempo 500000 )) Nothing -- tempo

               output h client port conn queue song' :: IO() -- MidiMessages
                                                        
               -- wait for end (needs h client port queue)
               Evt.drainOutput h
               Evt.outputPending h
               waitForEcho h client port
               putStrLn "done"                       -- cleanup
               Queue.control h queue Evt.QueueStop Nothing
               Port.delete h port


foo = open "xFLUID" >>= send song' >>= close
