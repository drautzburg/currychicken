import qualified Sound.MIDI.File as Mf
import qualified Sound.MIDI.File.Event as Me
import qualified Sound.MIDI.File.Load as Ml
import qualified Sound.MIDI.File.Save as Ms
import qualified Data.EventList.Relative.TimeBody as Tb
import qualified Sound.MIDI.File.Event as Evt

import CommonFormatting

{-
The header of a midi files begins with its type:
                
    data Type     = Mixed | Parallel | Serial
    Mixed    = 0  the file contains a single multi-channel track
    Parallel = 1  the file contains one or more simultaneous tracks (or MIDI outputs) of a sequence
    Serial   = 2  the file contains one or more sequentially independent single-track patterns

The next word, <ntrks>, is the number of track chunks in the file. It
will always be 1 for a format 0 file.

    It appears Mf.fromFile does not return this and why should it


-}                                             

-- track1 :: IO Mf.Track
track1 :: IO (Tb.T Mf.ElapsedTime Me.T)
track1 = do
    (Mf.Cons t1 d1 ts1) <- Ml.fromFile "GeorgyPorgy.mid"
    pretty (head ts1)
    return (head ts1)

{-
Mf.Track is just an alias for Tb.T Mf.ElapsedTime Me.T

TimeBody (Tb):
        Event lists starting with a time difference and ending with a body.

So this is essentially a list of times and values.

-}

