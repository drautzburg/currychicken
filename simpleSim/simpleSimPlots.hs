import SimpleSim
import Data.List
import Graphics.Gnuplot.Simple
import Graphics.Gnuplot.ColorSpecification
import qualified Graphics.EasyPlot as P

stCountsPerSegment :: Stream c -> [[(Time, Count)]]
stCountsPerSegment st = map f st
  where
    f (b,(t1,t2), tp) = [(t1,0),(t2, (t2-t1)*tp)]

xplotAll :: String -> Stream c -> IO()
xplotAll file st = plotListsStyle [EPS (file++".eps")] values
  where
    noCaption xs = (defaultStyle {lineSpec = CustomStyle [LineTitle "", LineWidth 2]},xs)
    times       = sort $ (stTimes st) ++ (map dt $ stTimes st)
      where
        dt t = t - 0.01 
    sums        = map (stCount st) times
    tps         = map ((*10) . stThroughput st) times
    values      = map noCaption (totals : throughputs: stCountsPerSegment st)
    totals      = zip times sums
    throughputs = zip times tps


plotAll :: String -> Stream c -> IO Bool
--plotAll file st = P.plot (P.PNG (file ++ ".png")) (
plotAll file st = P.plot P.X11 (
  plot "Totals" P.Black totals
  :
  plot "Throughputs" P.Red throughputs
  : segments
  )
  where
    times       = sort $ (stTimes st) ++ (map dt $ stTimes st)
      where
        dt t = t - 0.01
    plot t c dat = P.Data2D [P.Title t, P.Style P.Lines, P.Color c] [] dat
    sums        = map (stCount st) times
    tps         = map ((*10) . stThroughput st) times
    totals      = zip times sums
    throughputs = zip times tps
    segments    = map (plot "" P.Blue) (stCountsPerSegment st)


-- plotListsStyle [EPS "xxx.eps"] [(defaultStyle {lineSpec = CustomStyle [LineTitle ""]},[0,5..100::Double]) ]
main = do
  plotAll "summing" exStream1
  plotAll "throttle8" (stThrottle 8 exStream1)
  plotAll "throttle4" (stThrottle 4 exStream1)
  plotAll "parcel10" parcelSorterOutput10
