-- import Common
import GHC.Word
import Debug.Trace
import Sound.MIDI.ALSA
import Data.List
import Data.Function

-- Sound.ALSA.Sequencer
import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Port.Info as PortInfo
import qualified Sound.ALSA.Sequencer.Event as Event
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


handleExceptionCont :: ContT () IO () -> IO ()
handleExceptionCont = handleException . runContUnit

handleException :: IO () -> IO ()
handleException act =
   act
   `AlsaExc.catch` \e ->
      putStrLn $ "alsa_exception: " ++ AlsaExc.show e

runContUnit :: (Monad m) => ContT a m a -> m a
runContUnit cont = runContT cont return


{-
-- Tempo --

There are two parameters to define the actual tempo, PPQ (pulse per
quarter note) and MIDI tempo. The former defines the base resolution
of the ticks, while the latter defines the beat tempo in
microseconds. As default, 96 PPQ and 120 BPM are used,
respectively. That is, the tempo is set to 500000 (= 60 * 1000000 /
120). Note that PPQ cannot be changed while the queue is running. It
must be set before the queue is started.
-}


type Time = Word32


echo :: SndSeq.AllowOutput mode =>
     SndSeq.T mode -> Client.T -> Conn.T -> Queue.T -> Port.T -> Time -> IO Word
echo h client conn queue port t =
    let me = Addr.Cons client port
    in Event.output h (Event.forConnection conn (Event.CustomEv Event.Echo $ Event.Custom 0 0 0)){
        Event.queue = queue,
        Event.dest = me,
        Event.time = Time.consAbs $ Time.Tick t
        }

waitForEcho ::SndSeq.AllowInput mode =>
    SndSeq.T mode -> Client.T -> Port.T -> IO ()

waitForEcho h client port = let me = Addr.Cons client port
                            in do
                                event <- Event.input h                                
                                case Event.body event of
                                    Event.CustomEv Event.Echo _d ->
                                        if Event.source event == me
                                        then putStrLn $ "Received echo at t=" ++ (show $ Event.time event)
                                        else waitForEcho h client port
                                    _ -> waitForEcho h client port  


song' :: [(Time, Event.Data)]
song' = let q x = 96 * x
    in concatMap (\t -> [
    (q t + q 0,   Event.NoteEv Event.NoteOn  (Event.simpleNote (Event.Channel 0) (Event.Pitch 60) (Event.Velocity 42))),
    (q t + q 5,   Event.NoteEv Event.NoteOff (Event.simpleNote (Event.Channel 0) (Event.Pitch 60) (Event.Velocity 42))),
    (q t + q 1,   Event.NoteEv Event.NoteOn  (Event.simpleNote (Event.Channel 0) (Event.Pitch 64) (Event.Velocity 30))),
    (q t + q 5,   Event.NoteEv Event.NoteOff (Event.simpleNote (Event.Channel 0) (Event.Pitch 64) (Event.Velocity 30))),
    (q t + q 2,   Event.NoteEv Event.NoteOn  (Event.simpleNote (Event.Channel 0) (Event.Pitch 67) (Event.Velocity 40))),
    (q t + q 5,   Event.NoteEv Event.NoteOff (Event.simpleNote (Event.Channel 0) (Event.Pitch 67) (Event.Velocity 40))),
    (q t + q 3,   Event.NoteEv Event.NoteOn  (Event.simpleNote (Event.Channel 0) (Event.Pitch 71) (Event.Velocity 40))),
    (q t + q 6,   Event.NoteEv Event.NoteOff (Event.simpleNote (Event.Channel 0) (Event.Pitch 17) (Event.Velocity 40)))

    ]) [0,4 .. 12]


output :: SndSeq.AllowOutput mode =>
     SndSeq.T mode -> Client.T -> Port.T -> Conn.T -> Queue.T -> [(Time, Event.Data)]
     -> IO ()
output = output_t 0 100 



output_t :: SndSeq.AllowOutput mode =>
    Time -> Time -> SndSeq.T mode -> Client.T -> Port.T -> Conn.T -> Queue.T -> [(Time, Event.Data)]
    -> IO()
output_t  t dt h client port conn queue song =
    let (now, later) = break ((> t) . fst) song
        evt (t,e)    = (Event.forConnection conn e) {
                                Event.queue = queue,
                                Event.time = Time.consAbs $ Time.Tick t
                                }
        msg s t = putStrLn $ s ++ (show (length now)) ++ " events, up to t=" ++ show t
    in case later of
           [] -> do
               let endTime = maximum $ map fst song
               mapM_ (Event.output h . evt) now
               msg "playing last " endTime
               echo h client conn queue port (endTime+1)
               return ()
           _  -> do
               mapM_ (Event.output h . evt) now
               msg "playing " t
               output_t (t+dt) dt h client port conn queue later


createOutPort h name = Port.createSimple h name
            (Port.caps [Port.capRead, Port.capSubsRead, Port.capWrite])
            (Port.types [Port.typeMidiGeneric, Port.typeApplication])
    

main :: IO ()
main = handleExceptionCont $ liftIO $
    do
        h <- SndSeq.openDefault SndSeq.Block :: IO (SndSeq.T SndSeq.DuplexMode)
        Client.setName h "Gear"
        port <- createOutPort h "out"

        client <- Client.getId h 
        addr <- Addr.parse h "ESS"
        conn <- Conn.createTo h port addr
        queue <- Queue.alloc h
        Queue.control h queue Event.QueueStart Nothing
        Queue.control h queue (Event.QueueTempo (Event.Tempo 500000 )) Nothing

        output h client port conn queue song'
            -- wait for end
        _ <- Event.drainOutput h
        _ <- Event.outputPending h
        waitForEcho h client port
        putStrLn "done"
        Queue.control h queue Event.QueueStop Nothing
        Port.delete h port

