
\subsection{Cardano.BM.Counters.Linux}
\label{code:Cardano.BM.Counters.Linux}

%if style == newcode
\begin{code}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.BM.Counters.Linux
    ( readCounters
    , readResourceStats
    ) where

#ifdef ENABLE_OBSERVABLES
import           Data.Foldable (foldrM)
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Text (Text)
import qualified GHC.Stats as GhcStats
import           System.FilePath.Posix ((</>))
import           System.Posix.Files (getFileStatus,fileMode,ownerReadMode,
                     intersectFileModes)
import           System.Posix.Process (getProcessID)
import           System.Posix.Types (ProcessID)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)
import qualified Data.Text.Read as T (decimal)
#endif

#ifdef ENABLE_OBSERVABLES
import           Cardano.BM.Counters.Common (getMonoClock, readRTSStats)
import           Cardano.BM.Data.Observable
import           Cardano.BM.Data.Aggregated (Measurable(..))
import           Cardano.BM.Stats.Resources
#endif
import           Cardano.BM.Data.Counter
import           Cardano.BM.Data.SubTrace

\end{code}
%endif

\todo[inline]{we have to expand the |readMemStats| function\newline to read full data from |proc|}

\label{code:Linux.readCounters}\index{Counters!Linux!readCounters}
\begin{code}

readResourceStats :: IO (Maybe ResourceStats)
readResourceStats = do
  rts <- GhcStats.getRTSStats
  mkProcStats rts . fmap fromIntegral <$> readProcList "/proc/self/stat"
 where
   mkProcStats :: GhcStats.RTSStats -> [Word64] -> Maybe ResourceStats
   mkProcStats rts
               (_:_:_:_:_:_:_:_:_:_            -- 00-09
               :_:_:_:user:sys:_:_:_:_:threads -- 10-19
               :_:_:_:rss:_:_:_:_:_:_          -- 20-29
               :_:_:_:_:_:_:_:_:_:_            -- 30-39
               :_:blkio:_rest) =               -- 40-42
     Just $ Resources
       { rCentiCpu   = user + sys
       , rCentiGC    = nsToCenti $ GhcStats.gc_cpu_ns rts
       , rCentiMut   = nsToCenti $ GhcStats.mutator_cpu_ns rts
       , rGcsMajor   = fromIntegral $ GhcStats.major_gcs rts
       , rGcsMinor   = fromIntegral $ GhcStats.gcs rts - GhcStats.major_gcs rts
       , rAlloc      = GhcStats.allocated_bytes rts
       , rLive       = GhcStats.gcdetails_live_bytes $ GhcStats.gc rts
       , rHeap       = GhcStats.gcdetails_mem_in_use_bytes $ GhcStats.gc rts
       , rRSS        = rss * 4096 -- TODO:  this is really PAGE_SIZE.
       , rCentiBlkIO = blkio
       , rThreads    = threads
       }
   mkProcStats _ _ = Nothing
   nsToCenti :: GhcStats.RtsTime -> Word64
   nsToCenti = floor . (/ (10000000 :: Double)) . fromIntegral

readCounters :: SubTrace -> IO [Counter]
readCounters NoTrace                   = return []
readCounters Neutral                   = return []
readCounters (TeeTrace _)              = return []
readCounters (FilterTrace _)           = return []
readCounters UntimedTrace              = return []
readCounters DropOpening               = return []
readCounters (SetSeverity _)           = return []
#ifdef ENABLE_OBSERVABLES
readCounters (ObservableTraceSelf tts) = do
    pid <- getProcessID
    takeMeasurements pid tts
readCounters (ObservableTrace pid tts) =
    takeMeasurements pid tts

takeMeasurements :: ProcessID -> [ObservableInstance] -> IO [Counter]
takeMeasurements pid tts =
    foldrM (\(sel, fun) a ->
       if any (== sel) tts
       then (fun >>= \xs -> return $ a ++ xs)
       else return a) [] selectors
  where
    selectors = [ (MonotonicClock, getMonoClock)
                , (MemoryStats, readProcStatM pid)
                , (ProcessStats, readProcStats pid)
                , (NetStats, readProcNet pid)
                , (SysStats, readSysStats pid)
                , (IOStats, readProcIO pid)
                , (GhcRtsStats, readRTSStats)
                ]
#else
readCounters (ObservableTraceSelf _)   = return []
readCounters (ObservableTrace     _ _) = return []
#endif
\end{code}

\begin{code}
#ifdef ENABLE_OBSERVABLES
pathProc :: FilePath
pathProc = "/proc/"
pathProcStat :: ProcessID -> FilePath
pathProcStat pid = pathProc </> (show pid) </> "stat"
pathProcStatM :: ProcessID -> FilePath
pathProcStatM pid = pathProc </> (show pid) </> "statm"
pathProcIO :: ProcessID -> FilePath
pathProcIO pid = pathProc </> (show pid) </> "io"
pathProcNet :: ProcessID -> FilePath
pathProcNet pid = pathProc </> (show pid) </> "net" </> "netstat"
#endif
\end{code}

\subsubsection{Reading from a file in /proc/\textless pid \textgreater}

\begin{code}
#ifdef ENABLE_OBSERVABLES
readProcList :: FilePath -> IO [Integer]
readProcList fp = do
    fs <- getFileStatus fp
    if readable fs
    then do
        cs <- T.readFile fp
        return $ map (fromMaybe 0 . readMaybeText) (T.words cs)
    else
        return []
  where
    readable fs = intersectFileModes (fileMode fs) ownerReadMode == ownerReadMode
#endif
\end{code}

\subsubsection{readSysStats - generic platform specific information}
\begin{code}
#ifdef ENABLE_OBSERVABLES
readSysStats :: ProcessID -> IO [Counter]
readSysStats pid = do
    return [ Counter SysInfo "Pid" (PureI $ fromIntegral pid)
           , Counter SysInfo "Platform" (PureI $ fromIntegral $ fromEnum Linux)
           ]
#endif
\end{code}

\subsubsection{readProcStatM - /proc/\textless pid \textgreater/statm}

\begin{scriptsize}
\begin{verbatim}
/proc/[pid]/statm
       Provides information about memory usage, measured in pages.  The columns are:
              size       (1) total program size
                            (same as VmSize in /proc/[pid]/status)
              resident   (2) resident set size
                            (same as VmRSS in /proc/[pid]/status)
              shared     (3) number of resident shared pages (i.e., backed by a file)
                            (same as RssFile+RssShmem in /proc/[pid]/status)
              text       (4) text (code)
              lib        (5) library (unused since Linux 2.6; always 0)
              data       (6) data + stack
              dt         (7) dirty pages (unused since Linux 2.6; always 0)
\end{verbatim}
\end{scriptsize}

\begin{code}
#ifdef ENABLE_OBSERVABLES
readProcStatM :: ProcessID -> IO [Counter]
readProcStatM pid = do
    ps0 <- readProcList (pathProcStatM pid)
    let ps = zip colnames ps0
        psUseful = filter (("unused" /=) . fst) ps
    return $ map (\(n,i) -> Counter MemoryCounter n (PureI i)) psUseful
  where
    colnames :: [Text]
    colnames = ["size","resident","shared","text","unused","data","unused"]
#endif
\end{code}

\subsubsection{readProcStats - //proc//\textless pid \textgreater//stat}
\begin{scriptsize}
\begin{verbatim}
/proc/[pid]/stat
       Status  information about the process.  This is used by ps(1).  It is defined in the kernel source file
       fs/proc/array.c.

       The fields, in order, with their proper scanf(3) format specifiers, are listed below.  Whether  or  not
       certain   of   these   fields   display   valid  information  is  governed  by  a  ptrace  access  mode
       PTRACE_MODE_READ_FSCREDS | PTRACE_MODE_NOAUDIT check (refer to ptrace(2)).  If the check denies access,
       then the field value is displayed as 0.  The affected fields are indicated with the marking [PT].

       (1) pid  %d
                     The process ID.

       (2) comm  %s
                     The  filename  of  the  executable,  in parentheses.  This is visible whether or not the exe-
                     cutable is swapped out.

       (3) state  %c
                     One of the following characters, indicating process state:

                     R  Running

                     S  Sleeping in an interruptible wait

                     D  Waiting in uninterruptible disk sleep

                     Z  Zombie

                     T  Stopped (on a signal) or (before Linux 2.6.33) trace stopped

                     t  Tracing stop (Linux 2.6.33 onward)

                     W  Paging (only before Linux 2.6.0)

                     X  Dead (from Linux 2.6.0 onward)

                     x  Dead (Linux 2.6.33 to 3.13 only)

                     K  Wakekill (Linux 2.6.33 to 3.13 only)

                     W  Waking (Linux 2.6.33 to 3.13 only)

                     P  Parked (Linux 3.9 to 3.13 only)

       (4) ppid  %d
                     The PID of the parent of this process.

       (5) pgrp  %d
                     The process group ID of the process.

       (6) session  %d
                     The session ID of the process.

       (7) tty_nr  %d
                     The controlling terminal of the process.  (The minor device number is contained in the combi-
                     nation of bits 31 to 20 and 7 to 0; the major device number is in bits 15 to 8.)

       (8) tpgid  %d
                     The ID of the foreground process group of the controlling terminal of the process.

       (9) flags  %u
                     The  kernel  flags  word of the process.  For bit meanings, see the PF_* defines in the Linux
                     kernel source file include/linux/sched.h.  Details depend on the kernel version.

                     The format for this field was %lu before Linux 2.6.

       (10) minflt  %lu
                     The number of minor faults the process has made which have not required loading a memory page
                     from disk.

       (11) cminflt  %lu
                     The number of minor faults that the process's waited-for children have made.

       (12) majflt  %lu
                     The  number  of  major  faults the process has made which have required loading a memory page
                     from disk.

       (13) cmajflt  %lu
                     The number of major faults that the process's waited-for children have made.

       (14) utime  %lu
                     Amount of time that this process has been scheduled in user mode,  measured  in  clock  ticks
                     (divide by sysconf(_SC_CLK_TCK)).  This includes guest time, guest_time (time spent running a
                     virtual CPU, see below), so that applications that are not aware of the guest time  field  do
                     not lose that time from their calculations.

       (15) stime  %lu
                     Amount  of  time that this process has been scheduled in kernel mode, measured in clock ticks
                     (divide by sysconf(_SC_CLK_TCK)).

       (16) cutime  %ld
                     Amount of time that this process's waited-for children have been scheduled in user mode, mea-
                     sured  in  clock ticks (divide by sysconf(_SC_CLK_TCK)).  (See also times(2).)  This includes
                     guest time, cguest_time (time spent running a virtual CPU, see below).

       (17) cstime  %ld
                     Amount of time that this process's waited-for children have been scheduled  in  kernel  mode,
                     measured in clock ticks (divide by sysconf(_SC_CLK_TCK)).

       (18) priority  %ld
                     (Explanation  for  Linux  2.6)  For  processes  running a real-time scheduling policy (policy
                     below; see sched_setscheduler(2)), this is the negated scheduling priority, minus  one;  that
                     is,  a  number  in  the range -2 to -100, corresponding to real-time priorities 1 to 99.  For
                     processes running under a non-real-time scheduling policy, this is the raw nice  value  (set-
                     priority(2))  as  represented in the kernel.  The kernel stores nice values as numbers in the
                     range 0 (high) to 39 (low), corresponding to the user-visible nice range of -20 to 19.

       (19) nice  %ld
                     The nice value (see setpriority(2)), a value in the range 19 (low priority) to -20 (high pri-
                     ority).

       (20) num_threads  %ld
                     Number of threads in this process (since Linux 2.6).  Before kernel 2.6, this field was  hard
                     coded to 0 as a placeholder for an earlier removed field.

       (21) itrealvalue  %ld
                     The  time in jiffies before the next SIGALRM is sent to the process due to an interval timer.
                     Since kernel 2.6.17, this field is no longer maintained, and is hard coded as 0.

       (22) starttime  %llu
                     The time the process started after system boot.  In kernels before Linux 2.6, this value  was
                     expressed  in  jiffies.   Since  Linux  2.6, the value is expressed in clock ticks (divide by
                     sysconf(_SC_CLK_TCK)).

                     The format for this field was %lu before Linux 2.6.

       (23) vsize  %lu
                     Virtual memory size in bytes.

       (24) rss  %ld
                     Resident Set Size: number of pages the process has in real memory.  This is  just  the  pages
                     which  count  toward  text, data, or stack space.  This does not include pages which have not
                     been demand-loaded in, or which are swapped out.

       (25) rsslim  %lu
                     Current soft limit in bytes on the rss of the process; see the description of  RLIMIT_RSS  in
                     getrlimit(2).

       (26) startcode  %lu  [PT]
                     The address above which program text can run.

       (27) endcode  %lu  [PT]
                     The address below which program text can run.

       (28) startstack  %lu  [PT]
                     The address of the start (i.e., bottom) of the stack.

       (29) kstkesp  %lu  [PT]
                     The current value of ESP (stack pointer), as found in the kernel stack page for the process.

       (30) kstkeip  %lu  [PT]
                     The current EIP (instruction pointer).

       (31) signal  %lu
                     The  bitmap of pending signals, displayed as a decimal number.  Obsolete, because it does not
                     provide information on real-time signals; use /proc/[pid]/status instead.

       (32) blocked  %lu
                     The bitmap of blocked signals, displayed as a decimal number.  Obsolete, because it does  not
                     provide information on real-time signals; use /proc/[pid]/status instead.

       (33) sigignore  %lu
                     The  bitmap of ignored signals, displayed as a decimal number.  Obsolete, because it does not
                     provide information on real-time signals; use /proc/[pid]/status instead.

       (34) sigcatch  %lu
                     The bitmap of caught signals, displayed as a decimal number.  Obsolete, because it  does  not
                     provide information on real-time signals; use /proc/[pid]/status instead.

       (35) wchan  %lu  [PT]
                     This  is  the  "channel" in which the process is waiting.  It is the address of a location in
                     the kernel where the process is sleeping.  The corresponding symbolic name can  be  found  in
                     /proc/[pid]/wchan.

       (36) nswap  %lu
                     Number of pages swapped (not maintained).

       (37) cnswap  %lu
                     Cumulative nswap for child processes (not maintained).

       (38) exit_signal  %d  (since Linux 2.1.22)
                     Signal to be sent to parent when we die.

       (39) processor  %d  (since Linux 2.2.8)
                     CPU number last executed on.

       (40) rt_priority  %u  (since Linux 2.5.19)
                     Real-time  scheduling priority, a number in the range 1 to 99 for processes scheduled under a
                     real-time policy, or 0, for non-real-time processes (see sched_setscheduler(2)).

       (41) policy  %u  (since Linux 2.5.19)
                     Scheduling policy  (see  sched_setscheduler(2)).   Decode  using  the  SCHED_*  constants  in
                     linux/sched.h.

                     The format for this field was %lu before Linux 2.6.22.

       (42) delayacct_blkio_ticks  %llu  (since Linux 2.6.18)
                     Aggregated block I/O delays, measured in clock ticks (centiseconds).

       (43) guest_time  %lu  (since Linux 2.6.24)
                     Guest  time  of  the process (time spent running a virtual CPU for a guest operating system),
                     measured in clock ticks (divide by sysconf(_SC_CLK_TCK)).

       (44) cguest_time  %ld  (since Linux 2.6.24)
                     Guest  time   of   the   process's   children,   measured   in   clock   ticks   (divide   by
                     sysconf(_SC_CLK_TCK)).

       (45) start_data  %lu  (since Linux 3.3)  [PT]
                     Address above which program initialized and uninitialized (BSS) data are placed.

       (46) end_data  %lu  (since Linux 3.3)  [PT]
                     Address below which program initialized and uninitialized (BSS) data are placed.

       (47) start_brk  %lu  (since Linux 3.3)  [PT]
                     Address above which program heap can be expanded with brk(2).

       (48) arg_start  %lu  (since Linux 3.5)  [PT]
                     Address above which program command-line arguments (argv) are placed.

       (49) arg_end  %lu  (since Linux 3.5)  [PT]
                     Address below program command-line arguments (argv) are placed.

       (50) env_start  %lu  (since Linux 3.5)  [PT]
                     Address above which program environment is placed.

       (51) env_end  %lu  (since Linux 3.5)  [PT]
                     Address below which program environment is placed.

       (52) exit_code  %d  (since Linux 3.5)  [PT]
                     The thread's exit status in the form reported by waitpid(2).
\end{verbatim}
\end{scriptsize}

\begin{code}
#ifdef ENABLE_OBSERVABLES
readProcStats :: ProcessID -> IO [Counter]
readProcStats pid = do
    ps0 <- readProcList (pathProcStat pid)
    let ticks = if length ps0 > 15 then (ps0 !! 13 + ps0 !! 14) else 0
    let ps1 = zip colnames ps0
        ps2 = [("cputicks",ticks)] <> filter (("unused" /=) . fst) ps1
        metricWanted = \case
          0  -> True -- cputicks
          20 -> True -- numthreads
          24 -> True -- rss
          42 -> True -- blkio
          _ -> False
    return $ catMaybes $ map (\((val,i), nr) ->
                                if metricWanted nr
                                then Just $ Counter StatInfo val (PureI i)
                                else Nothing) $
                             zip ps2 [0::Int ..]
  where
    colnames :: [Text]
    colnames = [ "pid","unused","unused","ppid","pgrp","session","ttynr","tpgid","flags","minflt"
               , "cminflt","majflt","cmajflt","utime","stime","cutime","cstime","priority","nice","numthreads"
               , "itrealvalue","starttime","vsize","rss","rsslim","startcode","endcode","startstack","kstkesp","kstkeip"
               , "signal","blocked","sigignore","sigcatch","wchan","nswap","cnswap","exitsignal","processor","rtpriority"
               , "policy","blkio","guesttime","cguesttime","startdata","enddata","startbrk","argstart","argend","envstart"
               , "envend","exitcode"
               ]
#endif
\end{code}

\subsubsection{readProcIO - //proc//\textless pid \textgreater//io}
\begin{scriptsize}
\begin{verbatim}
/proc/[pid]/io (since kernel 2.6.20)
       This file contains I/O statistics for the process, for example:

              # cat /proc/3828/io
              rchar: 323934931
              wchar: 323929600
              syscr: 632687
              syscw: 632675
              read_bytes: 0
              write_bytes: 323932160
              cancelled_write_bytes: 0

       The fields are as follows:

       rchar: characters read
              The number of bytes which this task has caused to be read from storage.  This is simply the  sum
              of bytes which this process passed to read(2) and similar system calls.  It includes things such
              as terminal I/O and is unaffected by whether or not actual physical disk I/O was  required  (the
              read might have been satisfied from pagecache).

       wchar: characters written
              The  number  of bytes which this task has caused, or shall cause to be written to disk.  Similar
              caveats apply here as with rchar.

       syscr: read syscalls
              Attempt to count the number of read I/O operations-that is, system calls  such  as  read(2)  and
              pread(2).

       syscw: write syscalls
              Attempt  to  count the number of write I/O operations-that is, system calls such as write(2) and
              pwrite(2).

       read_bytes: bytes read
              Attempt to count the number of bytes which this process really did cause to be fetched from  the
              storage layer.  This is accurate for block-backed filesystems.

       write_bytes: bytes written
              Attempt to count the number of bytes which this process caused to be sent to the storage layer.

       cancelled_write_bytes:
              The  big  inaccuracy  here  is truncate.  If a process writes 1MB to a file and then deletes the
              file, it will in fact perform no writeout.  But it will have been accounted as having caused 1MB
              of  write.   In other words: this field represents the number of bytes which this process caused
              to not happen, by truncating pagecache.  A task can cause "negative"  I/O  too.   If  this  task
              truncates  some  dirty  pagecache,  some  I/O  which another task has been accounted for (in its
              write\_bytes) will not be happening.

       Note: In the current implementation, things are a bit racy  on  32-bit  systems:  if  process  A  reads
       process  B's  /proc/[pid]/io  while process B is updating one of these 64-bit counters, process A could
       see an intermediate result.

       Permission to access this file is governed by a ptrace access mode PTRACE\_MODE\_READ\_FSCREDS check;  see
       ptrace(2).
\end{verbatim}
\end{scriptsize}

\begin{code}
#ifdef ENABLE_OBSERVABLES
readProcIO :: ProcessID -> IO [Counter]
readProcIO pid = do

    ps0 <- readProcList (pathProcIO pid)
    let ps = zip3 colnames ps0 units
        ps2 = filter (\(n,_i,_u) -> "ign" /= n) ps
    return $ map (\(n,i,u) -> Counter IOCounter n (u i)) ps2
  where
    colnames :: [Text]
    colnames = [ "ign","rchar","ign","wchar","ign","syscr","ign","syscw","ign","rbytes","ign","wbytes","ign","cxwbytes" ]
    units    = [  PureI, Bytes . fromInteger, PureI, Bytes . fromInteger, PureI, PureI, PureI, PureI, PureI, Bytes . fromInteger, PureI, Bytes . fromInteger, PureI, Bytes . fromInteger ]
#endif
\end{code}

\subsubsection{Network TCP/IP counters}

\begin{scriptsize}
\begin{verbatim}
example:
\\
cat /proc/<pid>/net/netstat
\\
TcpExt: SyncookiesSent SyncookiesRecv SyncookiesFailed EmbryonicRsts PruneCalled RcvPruned OfoPruned OutOfWindowIcmps LockDroppedIcmps ArpFilter TW TWRecycled TWKilled PAWSActive PAWSEstab DelayedACKs DelayedACKLocked DelayedACKLost ListenOverflows ListenDrops TCPHPHits TCPPureAcks TCPHPAcks TCPRenoRecovery TCPSackRecovery TCPSACKReneging TCPSACKReorder TCPRenoReorder TCPTSReorder TCPFullUndo TCPPartialUndo TCPDSACKUndo TCPLossUndo TCPLostRetransmit TCPRenoFailures TCPSackFailures TCPLossFailures TCPFastRetrans TCPSlowStartRetrans TCPTimeouts TCPLossProbes TCPLossProbeRecovery TCPRenoRecoveryFail TCPSackRecoveryFail TCPRcvCollapsed TCPDSACKOldSent TCPDSACKOfoSent TCPDSACKRecv TCPDSACKOfoRecv TCPAbortOnData TCPAbortOnClose TCPAbortOnMemory TCPAbortOnTimeout TCPAbortOnLinger TCPAbortFailed TCPMemoryPressures TCPMemoryPressuresChrono TCPSACKDiscard TCPDSACKIgnoredOld TCPDSACKIgnoredNoUndo TCPSpuriousRTOs TCPMD5NotFound TCPMD5Unexpected TCPMD5Failure TCPSackShifted TCPSackMerged TCPSackShiftFallback TCPBacklogDrop PFMemallocDrop TCPMinTTLDrop TCPDeferAcceptDrop IPReversePathFilter TCPTimeWaitOverflow TCPReqQFullDoCookies TCPReqQFullDrop TCPRetransFail TCPRcvCoalesce TCPOFOQueue TCPOFODrop TCPOFOMerge TCPChallengeACK TCPSYNChallenge TCPFastOpenActive TCPFastOpenActiveFailTCPFastOpenPassive TCPFastOpenPassiveFail TCPFastOpenListenOverflow TCPFastOpenCookieReqd TCPFastOpenBlackhole TCPSpuriousRtxHostQueues BusyPollRxPackets TCPAutoCorking TCPFromZeroWindowAdv TCPToZeroWindowAdv TCPWantZeroWindowAdv TCPSynRetrans TCPOrigDataSent TCPHystartTrainDetect TCPHystartTrainCwnd TCPHystartDelayDetect TCPHystartDelayCwnd TCPACKSkippedSynRecv TCPACKSkippedPAWS TCPACKSkippedSeq TCPACKSkippedFinWait2 TCPACKSkippedTimeWait TCPACKSkippedChallenge TCPWinProbe TCPKeepAlive TCPMTUPFail TCPMTUPSuccess TCPDelivered TCPDeliveredCE TCPAckCompressed
TcpExt: 0 0 0 0 28 0 0 0 0 0 1670 1 0 0 6 6029 1 1766 0 0 384612 66799 105553 0 21 0 638 0 1 7 1 1 32 128 0 1 0 22 0 116 383 19 0 0 0 1788 224 178 0 435 224 0 13 0 0 0 0 0 0 67 0 0 0 0 3 1 668 0 0 0 4 0 0 0 0 0 91870 4468 0 224 22 23 0 0 0 0 0 0 0 6 0 21492 0 0 11 188 188680 6 145 13 425 0 3 4 0 0 1 117 22984 0 0 192495 0 4500
IpExt: InNoRoutes InTruncatedPkts InMcastPkts OutMcastPkts InBcastPkts OutBcastPkts InOctets OutOctets InMcastOctets OutMcastOctets InBcastOctets OutBcastOctets InCsumErrors InNoECTPkts InECT1Pkts InECT0Pkts InCEPkts
IpExt: 0 0 20053 8977 2437 23 3163525943 196480057 2426648 1491754 394285 5523 0 3513269 0 217426 0
\end{verbatim}
\end{scriptsize}

\begin{code}
#ifdef ENABLE_OBSERVABLES
readProcNet :: ProcessID -> IO [Counter]
readProcNet pid = do
  fields <- T.words . fourthLine . T.lines <$> T.readFile (pathProcNet pid)
  case -- We're only interested in 'InOctets' & 'OutOctets':
    fmap readMaybeText . take 2 . drop 7 $ fields of
      [Just netIn, Just netOut] -> return $ [ Counter NetCounter "IpExt:InOctets"  (Bytes netIn)
                                            , Counter NetCounter "IpExt:OutOctets" (Bytes netOut) ]
      _ -> pure []
  where
    -- Assumption: 'IpExt:' values are on the fourth line of how the kernel displays the buffer
    fourthLine ls = case drop 3 ls of
      l:_ -> l
      _   -> T.empty
#endif
\end{code}

\begin{code}
#ifdef ENABLE_OBSERVABLES
readMaybeText :: Integral a => T.Text -> Maybe a
readMaybeText t =
  case T.decimal t of
    Right (v, _)  -> Just v
    _             -> Nothing
#endif
\end{code}
