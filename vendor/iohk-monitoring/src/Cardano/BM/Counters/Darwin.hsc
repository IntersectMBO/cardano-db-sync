
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Cardano.BM.Counters.Darwin
    ( readCounters
    , readResourceStats
    , DiskInfo (..)
    ) where

#ifdef ENABLE_OBSERVABLES
import           Data.Foldable (foldrM)
import           Data.Word (Word32, Word8)
import           Data.Text (pack)
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import qualified GHC.Stats as GhcStats
import           System.Posix.Process (getProcessID)
import           System.Posix.Types (ProcessID)

import           Cardano.BM.Counters.Common (getMonoClock, readRTSStats)
import           Cardano.BM.Data.Observable
import           Cardano.BM.Data.Aggregated (Measurable(..))
import           Cardano.BM.Stats.Resources
#endif
import           Cardano.BM.Data.Counter
import           Cardano.BM.Data.SubTrace


#include "os-support-darwin.h"


{- type aliases -}
type MACH_VM_SIZE_T = Word64
data TIME_VALUE_T = TIME_VALUE_T Word64 Word64

{- memory information -}

{- mach/task_info.h
struct time_value {
	integer_t seconds;
	integer_t microseconds;
};
struct mach_task_basic_info {
	mach_vm_size_t  virtual_size;       /* virtual memory size (bytes) */
	mach_vm_size_t  resident_size;      /* resident memory size (bytes) */
	mach_vm_size_t  resident_size_max;  /* maximum resident memory size (bytes) */
	time_value_t    user_time;          /* total user run time for
	                                     *  terminated threads */
	time_value_t    system_time;        /* total system run time for
	                                     *  terminated threads */
	policy_t        policy;             /* default policy for new threads */
	integer_t       suspend_count;      /* suspend count for task */
}; -}

data MachTaskBasicInfo = MachTaskBasicInfo
  { _virtual_size :: !MACH_VM_SIZE_T
  , _resident_size :: !MACH_VM_SIZE_T
  , _resident_size_max :: !MACH_VM_SIZE_T
  , _user_time :: !TIME_VALUE_T
  , _system_time :: !TIME_VALUE_T
  , _policy :: !Word64
  , _suspend_count :: !Word64
  }

instance Storable TIME_VALUE_T where
  alignment _ = #const offsetof(struct {char x__; struct time_value (y__); }, y__)
  sizeOf _    = #size struct time_value
  peek ptr    = TIME_VALUE_T
                <$> (#peek struct time_value, seconds) ptr
                <*> (#peek struct time_value, microseconds) ptr
  poke _ _    = pure ()

instance Storable MachTaskBasicInfo where
  alignment _ = #const offsetof(struct {char x__; struct mach_task_basic_info (y__); }, y__)
  sizeOf _    = #size struct mach_task_basic_info
  peek ptr    = MachTaskBasicInfo
                <$> (#peek struct mach_task_basic_info, virtual_size) ptr
                <*> (#peek struct mach_task_basic_info, resident_size) ptr
                <*> (#peek struct mach_task_basic_info, resident_size_max) ptr
                <*> (#peek struct mach_task_basic_info, user_time) ptr
                <*> (#peek struct mach_task_basic_info, system_time) ptr
                <*> (#peek struct mach_task_basic_info, policy) ptr
                <*> (#peek struct mach_task_basic_info, suspend_count) ptr
  poke _ _    = pure ()

foreign import ccall unsafe c_get_process_memory_info :: Ptr MachTaskBasicInfo -> CInt -> IO CInt

{- host information -}

{- mach/host_info.h
struct host_basic_info {
	integer_t               max_cpus;               /* max number of CPUs possible */
	integer_t               avail_cpus;             /* number of CPUs now available */
	natural_t               memory_size;            /* size of memory in bytes, capped at 2 GB */
	cpu_type_t              cpu_type;               /* cpu type */
	cpu_subtype_t           cpu_subtype;            /* cpu subtype */
	cpu_threadtype_t        cpu_threadtype;         /* cpu threadtype */
	integer_t               physical_cpu;           /* number of physical CPUs now available */
	integer_t               physical_cpu_max;       /* max number of physical CPUs possible */
	integer_t               logical_cpu;            /* number of logical cpu now available */
	integer_t               logical_cpu_max;        /* max number of physical CPUs possible */
	uint64_t                max_mem;                /* actual size of physical memory */
}; -}

{- currently unused
data HostBasicInfo = HostBasicInfo
  { _max_cpus :: !CInt
  , _avail_cpus :: !CInt
  , _memory_size :: !CInt
  , _cpu_type :: !CInt
  , _cpu_subtype :: !CInt
  , _cpu_threadtype :: !CInt
  , _physical_cpu :: !CInt
  , _physical_cpu_max :: !CInt
  , _logical_cpu :: !CInt
  , _logical_cpu_max :: !CInt
  , _max_mem :: !Word64
  }

instance Storable HostBasicInfo where
  alignment _ = #const offsetof(struct {char x__; struct host_basic_info (y__); }, y__)
  sizeOf _    = #size struct host_basic_info
  peek ptr    = HostBasicInfo
                <$> (#peek struct host_basic_info, max_cpus) ptr
                <*> (#peek struct host_basic_info, avail_cpus) ptr
                <*> (#peek struct host_basic_info, memory_size) ptr
                <*> (#peek struct host_basic_info, cpu_type) ptr
                <*> (#peek struct host_basic_info, cpu_subtype) ptr
                <*> (#peek struct host_basic_info, cpu_threadtype) ptr
                <*> (#peek struct host_basic_info, physical_cpu) ptr
                <*> (#peek struct host_basic_info, physical_cpu_max) ptr
                <*> (#peek struct host_basic_info, logical_cpu) ptr
                <*> (#peek struct host_basic_info, logical_cpu_max) ptr
                <*> (#peek struct host_basic_info, max_mem) ptr
  poke _ _    = pure ()
 -}
-- foreign import ccall unsafe c_get_host_info :: Ptr HostBasicInfo -> IO CInt

foreign import ccall unsafe c_get_boot_time :: IO CInt

data CpuTimes = CpuTimes {
    _usertime :: Word64
  , _systime :: Word64
  , _idletime :: Word64
  , _nicetime :: Word64
  }

instance Storable CpuTimes where
  alignment _ = #const offsetof(struct {char x__; CPU_TIMES (y__); }, y__)
  sizeOf _    = #size CPU_TIMES
  peek ptr    = CpuTimes
                <$> (#peek CPU_TIMES, usertime) ptr
                <*> (#peek CPU_TIMES, systime) ptr
                <*> (#peek CPU_TIMES, idletime) ptr
                <*> (#peek CPU_TIMES, nicetime) ptr
  poke _ _    = pure ()

foreign import ccall unsafe c_get_sys_cpu_times :: Ptr CpuTimes -> IO CInt

data DiskInfo = DiskInfo {
    _reads       :: !Word64
  , _writes      :: !Word64
  , _read_bytes  :: !Word64
  , _write_bytes :: !Word64
  , _read_time   :: !Word64
  , _write_time  :: !Word64
  }
instance Storable DiskInfo where
  alignment _ = #const offsetof(struct {char x__; DISK_INFO (y__); }, y__)
  sizeOf _    = #size DISK_INFO
  peek ptr    = DiskInfo
                <$> (#peek DISK_INFO, reads) ptr
                <*> (#peek DISK_INFO, writes) ptr
                <*> (#peek DISK_INFO, read_bytes) ptr
                <*> (#peek DISK_INFO, write_bytes) ptr
                <*> (#peek DISK_INFO, read_time) ptr
                <*> (#peek DISK_INFO, write_time) ptr
  poke _ _    = pure ()

data DiskCounters = DiskCounters {
    _ndsks    :: !Word32
  -- , _dsknames :: ![String]
  , _dsks     :: ![DiskInfo]
  }
instance Storable DiskCounters where
  alignment _ = #const offsetof(struct {char x__; DISK_COUNTERS (y__); }, y__)
  sizeOf _    = #size DISK_COUNTERS
  peek ptr    = do
      n <- (#peek DISK_COUNTERS, ndsks) ptr
      -- names <- peekStrings (fromIntegral n) ((#ptr DISK_COUNTERS, dsknames) ptr)
      dskdata <- peekDiskInfo (fromIntegral n) ((#ptr DISK_COUNTERS, dsks) ptr)
      return $ DiskCounters n dskdata
  poke _ _    = pure ()

foreign import ccall unsafe c_get_sys_disk_io_counters :: Ptr DiskCounters -> IO CInt

peekDiskInfo :: Int -> Ptr DiskInfo -> IO [DiskInfo]
peekDiskInfo len ptr = peekDiskInfo' len []
  where
    peekDiskInfo' 0 acc = return acc
    peekDiskInfo' n acc = do
            let idx = len - n
            di <- peekElemOff ptr idx
            peekDiskInfo' (n - 1) (di : acc)


{- from net/if_var.h
/*
 * Structure describing information about an interface
 * which may be of interest to management entities.
 */
struct if_data {
	/* generic interface information */
	u_char          ifi_type;       /* ethernet, tokenring, etc */
	u_char          ifi_typelen;    /* Length of frame type id */
	u_char          ifi_physical;   /* e.g., AUI, Thinnet, 10base-T, etc */
	u_char          ifi_addrlen;    /* media address length */
	u_char          ifi_hdrlen;     /* media header length */
	u_char          ifi_recvquota;  /* polling quota for receive intrs */
	u_char          ifi_xmitquota;  /* polling quota for xmit intrs */
	u_char          ifi_unused1;    /* for future use */
	u_int32_t       ifi_mtu;        /* maximum transmission unit */
	u_int32_t       ifi_metric;     /* routing metric (external only) */
	u_int32_t       ifi_baudrate;   /* linespeed */
	/* volatile statistics */
	u_int32_t       ifi_ipackets;   /* packets received on interface */
	u_int32_t       ifi_ierrors;    /* input errors on interface */
	u_int32_t       ifi_opackets;   /* packets sent on interface */
	u_int32_t       ifi_oerrors;    /* output errors on interface */
	u_int32_t       ifi_collisions; /* collisions on csma interfaces */
	u_int32_t       ifi_ibytes;     /* total number of octets received */
	u_int32_t       ifi_obytes;     /* total number of octets sent */
	u_int32_t       ifi_imcasts;    /* packets received via multicast */
	u_int32_t       ifi_omcasts;    /* packets sent via multicast */
	u_int32_t       ifi_iqdrops;    /* dropped on input, this interface */
	u_int32_t       ifi_noproto;    /* destined for unsupported protocol */
	u_int32_t       ifi_recvtiming; /* usec spent receiving when timing */
	u_int32_t       ifi_xmittiming; /* usec spent xmitting when timing */
	struct IF_DATA_TIMEVAL ifi_lastchange;  /* time of last administrative change */
	u_int32_t       ifi_unused2;    /* used to be the default_proto */
	u_int32_t       ifi_hwassist;   /* HW offload capabilities */
	u_int32_t       ifi_reserved1;  /* for future use */
	u_int32_t       ifi_reserved2;  /* for future use */
};
 -}
data IfData64 = IfData64 {
    _ifi_type :: !Word8
  -- , _ifi_typelen :: !Word8
  -- , _ifi_physical :: !Word8
  -- , _ifi_addrlen :: !Word8
  -- , _ifi_hdrlen :: !Word8
  -- , _ifi_recvquota :: !Word8
  -- , _ifi_xmitquota :: !Word8
  -- , _ifi_unused1 :: !Word8
  , _ifi_mtu :: !Word32
  -- , _ifi_metric :: !Word32
  , _ifi_baudrate :: !Word32
  , _ifi_ipackets :: !Word32
  , _ifi_ierrors :: !Word32
  , _ifi_opackets :: !Word32
  , _ifi_oerrors :: !Word32
  , _ifi_collisions :: !Word32
  , _ifi_ibytes :: !Word32
  , _ifi_obytes :: !Word32
  , _ifi_imcasts :: !Word32
  , _ifi_omcasts :: !Word32
  -- , _ifi_iqdrops :: !Word32
  -- , _ifi_noproto :: !Word32
  -- , _ifi_recvtiming :: !Word32
  -- , _ifi_xmittiming :: !Word32
  -- , _ifi_lastchange :: !Word32
  -- , _ifi_unused2 :: !Word32
  -- , _ifi_hwassist :: !Word32
  -- , _ifi_reserved1 :: !Word32
  -- , _ifi_reserved2 :: !Word32
  }
instance Monoid IfData64 where
  mempty = IfData64 0 0 0 0 0 0 0 0 0 0 0 0
instance Semigroup IfData64 where
  (<>) (IfData64 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1)
       (IfData64 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2)  =
       (IfData64 (a1+a2) (b1+b2) (c1+c2) (d1+d2) (e1+e2) (f1+f2) (g1+g2) (h1+h2) (i1+i2) (j1+j2) (k1+k2) (l1+l2)) 
data NetIO = NetIO {
    _nifs    :: !Word32
  -- , _ifnames :: ![String]
  , _ifs     :: ![IfData64]
  }

instance Storable IfData64 where
  alignment _ = #const offsetof(struct {char x__; struct if_data64 (y__); }, y__)
  sizeOf _    = #size struct if_data64
  peek ptr    = IfData64
                <$> (#peek struct if_data64, ifi_type) ptr
                <*> (#peek struct if_data64, ifi_mtu) ptr
                <*> (#peek struct if_data64, ifi_baudrate) ptr
                <*> (#peek struct if_data64, ifi_ipackets) ptr
                <*> (#peek struct if_data64, ifi_ierrors) ptr
                <*> (#peek struct if_data64, ifi_opackets) ptr
                <*> (#peek struct if_data64, ifi_oerrors) ptr
                <*> (#peek struct if_data64, ifi_collisions) ptr
                <*> (#peek struct if_data64, ifi_ibytes) ptr
                <*> (#peek struct if_data64, ifi_obytes) ptr
                <*> (#peek struct if_data64, ifi_imcasts) ptr
                <*> (#peek struct if_data64, ifi_omcasts) ptr
  poke _ _    = pure ()

instance Storable NetIO where
  alignment _ = #const offsetof(struct {char x__; NET_IO (y__); }, y__)
  sizeOf _    = #size NET_IO
  peek ptr    = do
      n <- (#peek NET_IO, nifs) ptr
      -- names <- peekStrings (fromIntegral n) ((#ptr NET_IO, ifnames) ptr)
      ifdata <- peekIfData64 (fromIntegral n) ((#ptr NET_IO, ifs) ptr)
      return $ NetIO n ifdata
  poke _ _    = pure ()

foreign import ccall unsafe c_get_sys_network_io_counters :: Ptr NetIO -> IO CInt

peekIfData64 :: Int -> Ptr IfData64 -> IO [IfData64]
peekIfData64 len ptr = peekIfData64' len []
  where
    peekIfData64' 0 acc = return acc
    peekIfData64' n acc = do
        let idx = len - n
        ifd <- peekElemOff ptr idx
        peekIfData64' (n - 1) (ifd : acc)

{- peekStrings :: Int -> Ptr CString -> IO [String]
peekStrings len ptr = peekStrings' len []
  where
    peekStrings' 0 acc = return acc
    peekStrings' n acc = do
        let idx = len - n
        s <- peekCString =<< peekElemOff ptr idx
        peekStrings' (n - 1) (s : acc) -}

{-  -}

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
readCounters (ObservableTrace _pid _tts) = return []

takeMeasurements :: ProcessID -> [ObservableInstance] -> IO [Counter]
takeMeasurements pid tts =
    foldrM (\(sel, fun) a ->
       if any (== sel) tts
          then do
              xs <- fun
              return (a ++ xs)
          else pure a) [] selectors
  where
    selectors = [ (MonotonicClock, getMonoClock)
                , (SysStats, readSysStats pid)
                , (MemoryStats, readProcMem pid)
                , (ProcessStats, readProcStats pid)
                , (NetStats, readProcNet)
                , (IOStats, readProcIO)
                , (GhcRtsStats, readRTSStats)
                ]
#else
readCounters (ObservableTraceSelf _)   = return []
readCounters (ObservableTrace     _ _) = return []
#endif


#ifdef ENABLE_OBSERVABLES
usFromTimeValue :: TIME_VALUE_T -> Word64
usFromTimeValue (TIME_VALUE_T s us) = s * 1000000 + us

readProcMem :: ProcessID -> IO [Counter]
readProcMem pid = do
    meminfo <- getMemoryInfo pid
    return [ Counter MemoryCounter "Pid" (PureI $ fromIntegral pid)
           , Counter MemoryCounter "virtual_size" (Bytes $ fromIntegral (_virtual_size meminfo))
           , Counter MemoryCounter "resident_size" (Bytes $ fromIntegral (_resident_size meminfo))
           , Counter MemoryCounter "resident_size_max" (Bytes $ fromIntegral (_resident_size_max meminfo))
           ]

getMemoryInfo :: ProcessID -> IO MachTaskBasicInfo
getMemoryInfo pid =
  allocaBytes 128 $ \ptr -> do
    res <- c_get_process_memory_info ptr (fromIntegral pid)
    if res <= 0
      then do
        putStrLn $ "c_get_process_memory_info: failure returned: " ++ (show res)
        return $ MachTaskBasicInfo 0 0 0 (TIME_VALUE_T 0 0) (TIME_VALUE_T 0 0) 0 0
      else
        peek ptr

readResourceStats :: IO (Maybe ResourceStats)
readResourceStats = getProcessID >>= \pid -> do
  cpu <- getMemoryInfo pid
  rts <- GhcStats.getRTSStats
  mem <- getMemoryInfo pid
  pure . Just $
    Resources
    { rCentiCpu   = timeValToCenti (_user_time cpu)
                  + timeValToCenti (_system_time cpu)
    , rCentiGC    = nsToCenti $ GhcStats.gc_cpu_ns rts
    , rCentiMut   = nsToCenti $ GhcStats.mutator_cpu_ns rts
    , rGcsMajor   = fromIntegral $ GhcStats.major_gcs rts
    , rGcsMinor   = fromIntegral $ GhcStats.gcs rts - GhcStats.major_gcs rts
    , rAlloc      = GhcStats.allocated_bytes rts
    , rLive       = GhcStats.gcdetails_live_bytes $ GhcStats.gc rts
    , rHeap       = GhcStats.gcdetails_mem_in_use_bytes $ GhcStats.gc rts
    , rRSS        = fromIntegral (_resident_size mem)
    , rCentiBlkIO = 0
    , rThreads    = 0
    }
 where
   nsToCenti :: GhcStats.RtsTime -> Word64
   nsToCenti = fromIntegral . (`div` 10000000)
   timeValToCenti :: TIME_VALUE_T -> Word64
   timeValToCenti = fromIntegral . (`div` 10000) . usFromTimeValue

readSysStats :: ProcessID -> IO [Counter]
readSysStats _pid = do
    -- sysinfo <- getSysInfo
    cpuinfo <- getCpuInfo
    bootsecs <- c_get_boot_time
    return [
          --    Counter SysInfo "memory_size" (Bytes $ fromIntegral (_memory_size sysinfo))
          --  , Counter SysInfo "max_cpus" (PureI $ fromIntegral (_max_cpus sysinfo))
          --  , Counter SysInfo "avail_cpus" (PureI $ fromIntegral (_avail_cpus sysinfo))
          --  , Counter SysInfo "cpu_type" (PureI $ fromIntegral (_cpu_type sysinfo))
          --  , Counter SysInfo "cpu_subtype" (PureI $ fromIntegral (_cpu_subtype sysinfo))
          --  , Counter SysInfo "cpu_threadtype" (PureI $ fromIntegral (_cpu_threadtype sysinfo))
          --  , Counter SysInfo "physical_cpu" (PureI $ fromIntegral (_physical_cpu sysinfo))
          --  , Counter SysInfo "physical_cpu_max" (PureI $ fromIntegral (_physical_cpu_max sysinfo))
          --  , Counter SysInfo "logical_cpu" (PureI $ fromIntegral (_logical_cpu sysinfo))
          --  , Counter SysInfo "logical_cpu_max" (PureI $ fromIntegral (_logical_cpu_max sysinfo))
          --  , Counter SysInfo "max_mem" (Bytes $ fromIntegral (_max_mem sysinfo))
             Counter SysInfo "boot_time" (Seconds $ fromIntegral bootsecs)
           , Counter SysInfo "SysUserTime" (Nanoseconds $ fromIntegral (1000 * _usertime cpuinfo))
           , Counter SysInfo "SystemTime" (Nanoseconds $ fromIntegral (1000 * _systime cpuinfo))
           , Counter SysInfo "CPUTime" (Nanoseconds $ fromIntegral (1000 * (_systime cpuinfo + _usertime cpuinfo)))
           , Counter SysInfo "IdleTime" (Nanoseconds $ fromIntegral (1000 * _idletime cpuinfo))
           , Counter SysInfo "NiceTime" (Nanoseconds $ fromIntegral (1000 * _nicetime cpuinfo))
           , Counter SysInfo "Platform" (PureI $ fromIntegral $ fromEnum Darwin)
           ]
  where
    getCpuInfo :: IO CpuTimes
    getCpuInfo =
      allocaBytes 128 $ \ptr -> do
        res <- c_get_sys_cpu_times ptr
        if res <= 0
          then do
            putStrLn $ "c_get_sys_cpu_times: failure returned: " ++ (show res)
            return $ CpuTimes 0 0 0 0
          else
            peek ptr
  --   getSysInfo :: IO HostBasicInfo
  --   getSysInfo =
  --     allocaBytes 128 $ \ptr -> do
  --       res <- c_get_host_info ptr
  --       if res <= 0
  --         then do
  --           putStrLn $ "c_get_host_info: failure returned: " ++ (show res)
  --           return $ HostBasicInfo 0 0 0 0 0 0 0 0 0 0 0
  --         else
  --           peek ptr
#endif


#ifdef ENABLE_OBSERVABLES
readProcStats :: ProcessID -> IO [Counter]
readProcStats pid = do
    meminfo <- getMemoryInfo pid
    return [ Counter StatInfo "Pid" (PureI $ fromIntegral pid)
           , Counter StatInfo "user_time" (Nanoseconds $ 1000 * usFromTimeValue (_user_time meminfo))
           , Counter StatInfo "system_time" (Nanoseconds $ 1000 * usFromTimeValue (_system_time meminfo))
           , Counter StatInfo "policy" (PureI $ fromIntegral (_policy meminfo))
           , Counter StatInfo "suspend_count" (PureI $ fromIntegral (_suspend_count meminfo))
           ]
#endif


#ifdef ENABLE_OBSERVABLES
readProcIO :: IO [Counter]
readProcIO = do
    dskinfo <- getDiskInfo
    let ndsks = _ndsks dskinfo
        lsn = [ Counter IOCounter "ndisks" (PureI $ fromIntegral ndsks)]
        lscnt = map mkdskcounters (zip [0..32] (_dsks dskinfo))
        -- lsnms = map mkdskname (zip [0..32] (_dsknames dskinfo))
    return $ lsn <> concat lscnt
  where
    -- mkdskname :: (Int,String) -> Counter
    -- mkdskname (n,nm)=
    --   let ifnm = "dsk_" <> pack (show nm)
    --   in Counter IOCounter ifnm (PureI (fromIntegral n))
    mkdskcounters :: (Int,DiskInfo) -> [Counter]
    mkdskcounters (num,dta) =
      let nm = "dsk_" <> pack (show num)
      in [ Counter IOCounter (nm<>"-reads") (PureI $ fromIntegral (_reads dta))
         , Counter IOCounter (nm<>"-read_bytes") (Bytes $ fromIntegral (_read_bytes dta))
         , Counter IOCounter (nm<>"-writes") (PureI $ fromIntegral (_writes dta))
         , Counter IOCounter (nm<>"-write_bytes") (Bytes $ fromIntegral (_write_bytes dta))
         ]
    getDiskInfo :: IO DiskCounters
    getDiskInfo =
      allocaBytes (#size DISK_COUNTERS) $ \ptr -> do
        res <- c_get_sys_disk_io_counters ptr
        if res <= 0
          then do
            putStrLn $ "c_get_sys_disk_io_counters: failure returned: " ++ (show res)
            return $ DiskCounters 0 []
          else
            peek ptr
#endif


#ifdef ENABLE_OBSERVABLES
readProcNet :: IO [Counter]
readProcNet = do
    netinfo <- getNetInfo
    let n = _nifs netinfo
        lsn = [ Counter NetCounter "interfaces" (PureI $ fromIntegral n)]
        -- lscnt = map mkifcounters (zip [1..31] (_ifs netinfo))
        -- lsnms = map mkifname (zip [1..31] (_ifnames netinfo))
        lssum = mkifname (0, "summary")
              : mkifcounters (0, ifsum (_ifs netinfo))
    return $ lsn <> lssum
  where
    getNetInfo :: IO NetIO
    getNetInfo = do
      allocaBytes (#size NET_IO) $ \ptr -> do
        res <- c_get_sys_network_io_counters ptr
        if res <= 0
          then do
            putStrLn $ "c_get_sys_network_io_counters: failure returned: " ++ (show res)
            return $ NetIO 0 []
          else
            peek ptr
    ifsum :: [IfData64] -> IfData64
    ifsum = foldr (\ifd acc -> ifd <> acc) mempty
    mkifname :: (Int,String) -> Counter
    mkifname (n,nm)=
      let ifnm = "ifn_" <> pack (show nm)
      in Counter NetCounter ifnm (PureI (fromIntegral n))
    mkifcounters :: (Int,IfData64) -> [Counter]
    mkifcounters (num,dta) =
      let nm = "ifd_" <> pack (show num)
      in [ Counter NetCounter (nm<>"-mtu") (Bytes $ fromIntegral (_ifi_mtu dta))
         , Counter NetCounter (nm<>"-ipackets") (PureI $ fromIntegral (_ifi_ipackets dta))
         , Counter NetCounter (nm<>"-ierrors") (PureI $ fromIntegral (_ifi_ierrors dta))
         , Counter NetCounter (nm<>"-opackets") (PureI $ fromIntegral (_ifi_opackets dta))
         , Counter NetCounter (nm<>"-oerrors") (PureI $ fromIntegral (_ifi_oerrors dta))
         , Counter NetCounter (nm<>"-collisions") (PureI $ fromIntegral (_ifi_collisions dta))
         , Counter NetCounter (nm<>"-ibytes") (Bytes $ fromIntegral (_ifi_ibytes dta))
         , Counter NetCounter (nm<>"-obytes") (Bytes $ fromIntegral (_ifi_obytes dta))
         , Counter NetCounter (nm<>"-imcast") (PureI $ fromIntegral (_ifi_imcasts dta))
         , Counter NetCounter (nm<>"-omcast") (PureI $ fromIntegral (_ifi_omcasts dta))
         ]

#endif
