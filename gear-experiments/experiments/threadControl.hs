{-
We may have to start one thread per midi device. How do we start and
stop thesethreads?
-}

import Control.Monad
import Control.Concurrent
import System.IO
import Data.Maybe
        
-- A play thread




-- | Each track contains a list of strings. In the real world these
-- will be midi messages

data Track = Track {
    tName :: Name,
    tMsgs :: [MidiMessage]
    }
type Name = String
type MidiMessage = String

            


-- | This is the active code behind a Device. It sends the raw Midi
-- messages to the hardware and answers with a response. This is a
-- dummy implementation. In the real world the types for raw midi
-- messages and for the way we communicate problems and successes to
-- our callers, may well be different.
writeRawMidi :: [RawMidiMsg]-> IO(RawMidiResp)
writeRawMidi msgs = do
    (return . unwords) msgs
type RawMidiMsg = MidiMessage
type RawMidiResp = String
                 
-- | The raw IO code is wrapped inside two MVwars, one input and one
-- output. This allows us to start several of these via forkIO and we
-- get threads. A thread will wait, when it asks for input, but there
-- is none, or when it wants to write a response, but its previous
-- response has not yet been picked up by anybody. This code is the
-- "inside" of the arrangement: "some code between two MVars".
writeMidiMv :: MVar ControlCmd -> MVar ControlResponse -> IO ()
writeMidiMv mvIn mvOut = do
    actions <- takeMVar mvIn
    case actions of
        Write msgs -> do
                     -- RawMidiResp -> ControlResponse
            writeRawMidi msgs >>= (putMVar mvOut . Debug) 
            writeMidiMv mvIn mvOut            -- loop and wait for more 
        Stop -> do
            putMVar mvOut Stopped      -- Will not read mvIn after this (xxx useless?)
data ControlCmd = Write [MidiMessage] | Stop deriving (Show)
data ControlResponse = Ok | Stopped | Debug String deriving (Show)


-- | Dummy track containing dummy messages
mkTrack :: Int -> String -> Track
mkTrack i name = Track name ["msg-" ++ show i | i <- [0..i]]


-- | A Device is the "outside" of the arrangement: "some code between
-- two MVars". It exposes the two MVars and holds the TheadId of the
-- thread inside. The ThreadId is not used anywhere, currently.

data Device = Device {
            dIn :: MVar ControlCmd,
            dOut :: MVar ControlResponse,
            dThread :: ThreadId
        }

-- | This sets up a device with its two MVars and the thread
-- inside. Note, that is doesn't take parameters. Also it is unnamed.
mkDevice :: IO Device
mkDevice = do
    mvIn   <- newEmptyMVar
    mvOut  <- newEmptyMVar
    thread <- forkIO $ writeMidiMv mvIn mvOut
    return $ Device mvIn mvOut thread

    
-- | In the end, we want to play Tracks and for each Track, there
-- needs to be a Device. Someone will have to cut up the track is sets
-- of MidiMessages, which can be sent to a Device in one go. This
-- process will eat up the track eventually.
data Control = Control {  
    cName :: Name,
    cData :: Track,
    cDev  :: Device
    }
instance Show Control where
        show c = cName c ++ "(" ++ (tName . cData) c ++ ") "


-- | To make a control, we need to know its track and we need to
-- create a Device. Given the ting a name also won't hurt.
mkControl :: String -> Track -> IO Control
mkControl name track = mkDevice >>= (return . Control name track)


           
-- | If we have a control, we can talk to the thread inside.
writeControl :: Control -> ControlCmd -> IO ()
writeControl = putMVar . dIn . cDev

readControl :: Control -> IO (ControlResponse)
readControl = takeMVar . dOut . cDev



-- | Tell a Control to play the next chunk of messages
sendNextChunk :: Control -> IO (Maybe Control)
sendNextChunk ctrl
              | null now  = return Nothing -- no more data
              | otherwise = writeControl ctrl (Write now) >> (return .Just) ctrl'
        where track = cData ctrl
              (now, later) = (splitAt 3 . tMsgs) track
              ctrl' = ctrl{cData = track{tMsgs=later}}


sendStop :: Control -> IO ()
sendStop ctrl = writeControl ctrl Stop


-- | Write Response to stderr, for debugging
printControlResponse :: Control -> IO ()
printControlResponse ctrl = do
    outVal <- readControl ctrl
    hPutStrLn stderr $ show ctrl ++ show outVal


justControls :: [Maybe Control] -> [Control]
justControls = map fromJust . filter isJust

cycleCtrls :: [Control] -> IO [Control]
cycleCtrls controls = do
    ctrls' <- mapM sendNextChunk controls
    let jc = justControls ctrls'
    mapM printControlResponse jc
    return jc


playTracks :: IO [Control] -> IO ()
playTracks ioctrl = do
    controls <- ioctrl
    ctrls' <- cycleCtrls controls
    putStrLn ""
    if (null ctrls') then return () else playTracks (return ctrls')


tracks :: [Track]
tracks = [ mkTrack 1 "track1",
           mkTrack 5 "track2",
           mkTrack 10 "track3"
         ]
controls :: [Track -> IO Control]
controls = [ mkControl "Ess        ",
             mkControl "Audiophile ",
             mkControl "QSynth     "
          ]


zipControlsAndTracks :: [Track -> IO Control] -> [Track] -> IO [Control]
zipControlsAndTracks controls tracks
        | length controls < length tracks = error "not enough controls"
        | otherwise = mapM f (zip controls tracks)
        where
            f (p, t) = p t



xxx = let ioCtrl = zipControlsAndTracks controls tracks
      in playTracks ioCtrl >> putStrLn "done"
          
    

