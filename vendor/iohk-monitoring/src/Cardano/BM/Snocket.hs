--
-- copied from https://github.com/input-output-hk/ouroboros-network
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.BM.Snocket
  ( -- * Snocket Interface
    Accept (..)
  , fmapAccept
  , mkListeningSocket
  , AddressFamily (..)
  , Snocket (..)
    -- ** Socket based Snocktes
  , SocketSnocket
  , socketSnocket
    -- ** Local Snockets
    -- Using unix sockets (posix) or named pipes (windows)
    --
  , LocalSnocket
  , localSnocket
  , LocalAddress (..)
  , LocalFD
  , localAddressFromPath
  , localFDToHandle
  ) where

import           Control.Exception
import           Control.Monad (when)
import           Network.Socket (SockAddr (..), Socket)
import qualified Network.Socket as Socket
#if defined(mingw32_HOST_OS)
import           Data.Bits
import qualified System.Win32 as Win32
import qualified System.Win32.Async as Win32.Async
import qualified System.Win32.NamedPipes as Win32
import           System.Win32.Types (hANDLEToHandle)
#endif

import qualified System.IO as IO

import           Cardano.BM.IOManager


-- | Named pipes and Berkeley sockets have different API when accepting
-- a connection.  For named pipes the file descriptor created by 'createNamedPipe' is
-- supposed to be used for the first connected client.  Named pipe accept loop
-- looks this way:
--
-- > acceptLoop k = do
-- >   h <- createNamedPipe name
-- >   connectNamedPipe h
-- >   -- h is now in connected state
-- >   forkIO (k h)
-- >   acceptLoop k
--
-- For Berkeley sockets equivalent loop starts by creating a socket
-- which accepts connections and accept returns a new socket in connected
-- state
--
-- > acceptLoop k = do
-- >     s <- socket ...
-- >     bind s address
-- >     listen s
-- >     loop s
-- >   where
-- >     loop s = do
-- >       (s' , _addr') <- accept s
-- >       -- s' is in connected state
-- >       forkIO (k s')
-- >       loop s
--
-- To make common API for both we use a recursive type 'Accept', see
-- 'berkeleyAccept' below.  Creation of a socket / named pipe is part of
-- 'Snocket', but this means we need to have different recursion step for named
-- pipe & sockets.  For sockets its recursion step will always return 'accept'
-- syscall; for named pipes the first callback will reuse the file descriptor
-- created by 'open' and only subsequent calls will create a new file
-- descriptor by `createNamedPipe`, see 'namedPipeSnocket'.
--
newtype Accept addr fd = Accept
  { runAccept :: IO (fd, addr, Accept addr fd)
  }


-- | Arguments of 'Accept' are in the wrong order.
--
-- TODO: this can be fixed later.
--
fmapAccept :: (addr -> addr') -> Accept addr fd -> Accept addr' fd
fmapAccept f ac = Accept $ g <$> runAccept ac
  where
    g (fd, addr, next) = (fd, f addr, fmapAccept f next)


mkListeningSocket
    :: Snocket IO fd addr
    -> Maybe addr
    -> AddressFamily addr
    -> IO fd
mkListeningSocket sn addr family_ = do
    sd <- open sn family_

    case addr of
      Nothing -> pure ()
      Just addr_ -> do
        bind sn sd addr_
        listen sn sd
    pure sd

-- | BSD accept loop.
--
berkeleyAccept :: IOManager
               -> Socket
               -> Accept SockAddr Socket
berkeleyAccept ioManager sock = go
    where
      go = Accept $ do
        (sock', addr') <-
#if !defined(mingw32_HOST_OS)
          Socket.accept sock
#else
          Win32.Async.accept sock
#endif
        associateWithIOManager ioManager (Right sock')
          `catch` \(e :: IOException) -> do
            Socket.close sock'
            throwIO e
          `catch` \(SomeAsyncException _) -> do
            Socket.close sock'
            throwIO e
        return (sock', addr', go)

-- | Local address, on Unix is associated with `Socket.AF_UNIX` family, on
--
-- Windows with `named-pipes`.
--
newtype LocalAddress = LocalAddress { getFilePath :: FilePath }
  deriving (Show, Eq, Ord)


-- | We support either sockets or named pipes.
--
data AddressFamily addr where

    SocketFamily :: !Socket.Family
                 -> AddressFamily Socket.SockAddr

    LocalFamily  :: AddressFamily LocalAddress

instance Eq (AddressFamily addr) where
    SocketFamily fam0 == SocketFamily fam1 = fam0 == fam1
    LocalFamily       == LocalFamily       = True

instance Show (AddressFamily addr) where
    show (SocketFamily fam) = show fam
    show LocalFamily        = "LocalFamily"

-- | Abstract communication interface that can be used by more than
-- 'Socket'.  Snockets are polymorphic over monad which is used, this feature
-- is useful for testing and/or simulations.
--
data Snocket m fd addr
  = Snocket
      { getLocalAddr  :: fd -> m addr
      , getRemoteAddr :: fd -> m addr
      , addrFamily    :: addr -> AddressFamily addr
      , open          :: AddressFamily addr -> m fd
        -- |^ Open a file descriptor: socket='socket', namedPipe='CreateNamedPipe'
      , openToConnect :: addr -> m fd
        -- |^ A way to create 'fd' to pass to 'connect'.  For named pipes it uses 'CreateFile'
      , connect       :: fd -> addr -> m ()
        -- |^ `connect` is only needed for Berkeley sockets, for named pipes this is a no-op.
      , bind          :: fd -> addr -> m ()
      , listen        :: fd -> m ()
      , accept        :: fd -> Accept addr fd
      , close         :: fd -> m ()
      }


--
-- Socket based Snockets
--


socketAddrFamily
    :: Socket.SockAddr
    -> AddressFamily Socket.SockAddr
socketAddrFamily (Socket.SockAddrInet  _ _    ) = SocketFamily Socket.AF_INET
socketAddrFamily (Socket.SockAddrInet6 _ _ _ _) = SocketFamily Socket.AF_INET6
socketAddrFamily (Socket.SockAddrUnix _       ) = SocketFamily Socket.AF_UNIX


type SocketSnocket = Snocket IO Socket SockAddr


-- | Create a 'Snocket' for the given 'Socket.Family'. In the 'bind' method set
-- 'Socket.ReuseAddr` and 'Socket.ReusePort'.
--
socketSnocket
  :: IOManager
  -- ^ 'IOManager' interface.  We use it when we create a new socket and when we
  -- accept a connection.
  --
  -- Though it could be used in `open`, but that is going to be used in
  -- a bracket so it's better to keep it simple.
  --
  -> SocketSnocket
socketSnocket ioManager = Snocket {
      getLocalAddr   = Socket.getSocketName
    , getRemoteAddr  = Socket.getPeerName
    , addrFamily     = socketAddrFamily
    , open           = openSocket
    , openToConnect  = \addr -> openSocket (socketAddrFamily addr)
    , connect        = \s a -> do
#if !defined(mingw32_HOST_OS)
        Socket.connect s a
#else
        Win32.Async.connect s a
#endif
    , bind = \sd addr -> do
        let SocketFamily fml = socketAddrFamily addr
        when (fml == Socket.AF_INET ||
              fml == Socket.AF_INET6) $ do
          Socket.setSocketOption sd Socket.ReuseAddr 1
#if !defined(mingw32_HOST_OS)
          -- not supported on Windows 10
          Socket.setSocketOption sd Socket.ReusePort 1
#endif
        when (fml == Socket.AF_INET6)
          -- An AF_INET6 socket can be used to talk to both IPv4 and IPv6 end points, and
          -- it is enabled by default on some systems. Disabled here since we run a separate
          -- IPv4 server instance if configured to use IPv4.
          $ Socket.setSocketOption sd Socket.IPv6Only 1

        Socket.bind sd addr
    , listen   = \s -> Socket.listen s 8
    , accept   = berkeleyAccept ioManager
    , close    = Socket.close
    }
  where

    openSocket :: AddressFamily SockAddr -> IO Socket
    openSocket (SocketFamily family_) = do
      sd <- Socket.socket family_ Socket.Stream Socket.defaultProtocol
      associateWithIOManager ioManager (Right sd)
        -- open is designed to be used in `bracket`, and thus it's called with
        -- async exceptions masked.  The 'associteWithIOCP' is a blocking
        -- operation and thus it may throw.
        `catch` \(e :: IOException) -> do
          Socket.close sd
          throwIO e
        `catch` \(SomeAsyncException _) -> do
          Socket.close sd
          throwIO e
      return sd



--
-- NamedPipes based Snocket
--


#if defined(mingw32_HOST_OS)
type HANDLESnocket = Snocket IO Win32.HANDLE LocalAddress

-- | Create a Windows Named Pipe Snocket.
--
namedPipeSnocket
  :: IOManager
  -> FilePath
  -> HANDLESnocket
namedPipeSnocket ioManager path = Snocket {
      getLocalAddr  = \_ -> return localAddress
    , getRemoteAddr = \_ -> return localAddress
    , addrFamily  = \_ -> LocalFamily

    , open = \_addrFamily -> do
        hpipe <- Win32.createNamedPipe
                   path
                   (Win32.pIPE_ACCESS_DUPLEX .|. Win32.fILE_FLAG_OVERLAPPED)
                   (Win32.pIPE_TYPE_BYTE .|. Win32.pIPE_READMODE_BYTE)
                   Win32.pIPE_UNLIMITED_INSTANCES
                   65536   -- outbound pipe size
                   16384   -- inbound pipe size
                   0       -- default timeout
                   Nothing -- default security
        associateWithIOManager ioManager (Left hpipe)
          `catch` \(e :: IOException) -> do
            Win32.closeHandle hpipe
            throwIO e
          `catch` \(SomeAsyncException _) -> do
            Win32.closeHandle hpipe
            throwIO e
        pure hpipe

    -- To connect, simply create a file whose name is the named pipe name.
    , openToConnect  = \(LocalAddress pipeName) -> do
        hpipe <- Win32.connect pipeName
                   (Win32.gENERIC_READ .|. Win32.gENERIC_WRITE )
                   Win32.fILE_SHARE_NONE
                   Nothing
                   Win32.oPEN_EXISTING
                   Win32.fILE_FLAG_OVERLAPPED
                   Nothing
        associateWithIOManager ioManager (Left hpipe)
          `catch` \(e :: IOException) -> do
            Win32.closeHandle hpipe
            throwIO e
          `catch` \(SomeAsyncException _) -> do
            Win32.closeHandle hpipe
            throwIO e
        return hpipe
    , connect  = \_ _ -> pure ()

    -- Bind and listen are no-op.
    , bind     = \_ _ -> pure ()
    , listen   = \_ -> pure ()

    , accept   = \hpipe -> Accept $ do
          Win32.Async.connectNamedPipe hpipe
          return (hpipe, localAddress, acceptNext)

    , close    = Win32.closeHandle
    }
  where
    localAddress :: LocalAddress
    localAddress = LocalAddress path

    acceptNext :: Accept LocalAddress Win32.HANDLE
    acceptNext = Accept $ do
      hpipe <- Win32.createNamedPipe
                 path
                 (Win32.pIPE_ACCESS_DUPLEX .|. Win32.fILE_FLAG_OVERLAPPED)
                 (Win32.pIPE_TYPE_BYTE .|. Win32.pIPE_READMODE_BYTE)
                 Win32.pIPE_UNLIMITED_INSTANCES
                 65536   -- outbound pipe size
                 16384   -- inbound pipe size
                 0       -- default timeout
                 Nothing -- default security
              `catch` \(e :: IOException) -> do
                 putStrLn $ "accept: " ++ show e
                 throwIO e
      associateWithIOManager ioManager (Left hpipe)
      Win32.Async.connectNamedPipe hpipe
      return (hpipe, localAddress, acceptNext)
#endif


--
-- Windows/POSIX type aliases
--

localSnocket :: IOManager -> FilePath -> LocalSnocket
localFDToHandle :: LocalFD -> IO IO.Handle

-- | System dependent LocalSnocket type
#if defined(mingw32_HOST_OS)
type LocalSnocket = HANDLESnocket
type LocalFD      = Win32.HANDLE

localSnocket = namedPipeSnocket
localFDToHandle = hANDLEToHandle
#else
type LocalSnocket = Snocket IO Socket LocalAddress
type LocalFD      = Socket

localSnocket ioManager _ = Snocket {
      getLocalAddr  = fmap toLocalAddress . Socket.getSocketName
    , getRemoteAddr = fmap toLocalAddress . Socket.getPeerName
    , addrFamily    = const LocalFamily
    , connect       = \s addr -> do
        Socket.connect s (fromLocalAddress addr)
    , bind          = \fd addr -> Socket.bind fd (fromLocalAddress addr)
    , listen        = flip Socket.listen 1
    , accept        = fmapAccept toLocalAddress . (berkeleyAccept ioManager)
    , open          = openSocket
    , openToConnect = \_addr -> openSocket LocalFamily
    , close         = Socket.close
    }
  where
    toLocalAddress :: SockAddr -> LocalAddress
    toLocalAddress (SockAddrUnix path) = LocalAddress path
    toLocalAddress _                   = error "localSnocket.toLocalAddr: impossible happend"

    fromLocalAddress :: LocalAddress -> SockAddr
    fromLocalAddress = SockAddrUnix . getFilePath

    openSocket :: AddressFamily LocalAddress -> IO Socket
    openSocket LocalFamily = do
      sd <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
      associateWithIOManager ioManager (Right sd)
        -- open is designed to be used in `bracket`, and thus it's called with
        -- async exceptions masked.  The 'associteWithIOManager' is a blocking
        -- operation and thus it may throw.
        `catch` \(e :: IOException) -> do
          Socket.close sd
          throwIO e
        `catch` \(SomeAsyncException _) -> do
          Socket.close sd
          throwIO e
      return sd

localFDToHandle = flip Socket.socketToHandle IO.ReadWriteMode
#endif

localAddressFromPath :: FilePath -> LocalAddress
localAddressFromPath = LocalAddress
