
\subsection{Cardano.BM.Counters.Common}
\label{code:Cardano.BM.Counters.Common}

Common functions that serve |readCounters| on all platforms.

%if style == newcode
\begin{code}
module Cardano.BM.Counters.Common
    (
    --   nominalTimeToMicroseconds
      getMonoClock
    , readRTSStats
    ) where

import           Data.Text (Text)

import           GHC.Clock (getMonotonicTimeNSec)
import qualified GHC.Stats as GhcStats

import           Cardano.BM.Data.Aggregated (Measurable (..))
import           Cardano.BM.Data.Counter

\end{code}
%endif

\label{code:nominalTimeToMicroseconds}\index{nominalTimeToMicroseconds}
\begin{spec}
nominalTimeToMicroseconds :: Word64 -> Microsecond
nominalTimeToMicroseconds = fromMicroseconds . toInteger . (`div` 1000)
\end{spec}

\subsubsection{Read monotonic clock}\label{code:getMonoClock}\index{getMonoClock}
\begin{code}
getMonoClock :: IO [Counter]
getMonoClock = do
    t <- getMonotonicTimeNSec
    return [ Counter MonotonicClockTime "monoclock" $ Microseconds (t `div` 1000) ]

\end{code}

\subsubsection{Read GHC RTS statistics}\label{code:readRTSStats}\index{readRTSStats}
Read counters from GHC's |RTS| (runtime system). The values returned are as per the last
GC (garbage collection) run.
\begin{code}
readRTSStats :: IO [Counter]
readRTSStats = do
    iscollected <- GhcStats.getRTSStatsEnabled
    if iscollected
        then ghcstats
        else return []
  where
    ghcstats :: IO [Counter]
    ghcstats = do
        -- need to run GC?
        rts <- GhcStats.getRTSStats
        let getrts = ghcval rts
        return [ getrts (Bytes . fromIntegral . GhcStats.allocated_bytes, "bytesAllocated")
               , getrts (Bytes . fromIntegral . GhcStats.cumulative_live_bytes, "liveBytes")
               , getrts (Bytes . fromIntegral . GhcStats.max_live_bytes, "maxLiveBytes")
               , getrts (Bytes . fromIntegral . GhcStats.max_large_objects_bytes, "maxLargeBytes")
               , getrts (Bytes . fromIntegral . GhcStats.max_compact_bytes, "maxCompactBytes")
               , getrts (Bytes . fromIntegral . GhcStats.max_slop_bytes, "maxSlopBytes")
               , getrts (Bytes . fromIntegral . GhcStats.max_mem_in_use_bytes, "maxUsedMemBytes")
               , getrts (Bytes . fromIntegral . GhcStats.gcdetails_live_bytes . GhcStats.gc, "gcLiveBytes")
               , getrts (Bytes . fromIntegral . GhcStats.gcdetails_copied_bytes . GhcStats.gc, "gcCopiedBytes")
               , getrts (Nanoseconds . fromIntegral . GhcStats.gc_cpu_ns, "gcCpuNs")
               , getrts (Nanoseconds . fromIntegral . GhcStats.gc_elapsed_ns, "gcElapsedNs")
               , getrts (Nanoseconds . fromIntegral . GhcStats.cpu_ns, "cpuNs")
               , getrts (Nanoseconds . fromIntegral . GhcStats.elapsed_ns, "elapsedNs")
               , getrts (PureI . toInteger . GhcStats.gcs, "gcNum")
               , getrts (PureI . toInteger . GhcStats.major_gcs, "gcMajorNum")
               ]
    ghcval :: GhcStats.RTSStats -> ((GhcStats.RTSStats -> Measurable), Text) -> Counter
    ghcval s (f, n) = Counter RTSStats n $ (f s)
\end{code}
