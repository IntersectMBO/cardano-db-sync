
\subsection{Cardano.BM.Rotator}
\label{code:Cardano.BM.Rotator}

Implementation of rotation of logging files.

Monitor log files for max age and max size. This test only works on POSIX platforms.

\begin{code}

{-# LANGUAGE CPP             #-}

#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

#if !defined(mingw32_HOST_OS)
#define POSIX
#endif

module Cardano.BM.Rotator
       ( cleanupRotator
       , evalRotator
       , initializeRotator
       , latestLogFile
       , prtoutException
       , nameLogFile
       , tsformat
       , listLogFiles
       ) where

import           Control.Exception.Safe (Exception (..), catchIO)
#ifdef POSIX
import           Control.Monad (when)
#endif
import           Data.List (sort)
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty (NonEmpty)
import           Data.Time (UTCTime, addUTCTime, diffUTCTime, getCurrentTime,
                     parseTimeM)
import           Data.Time.Format (defaultTimeLocale, formatTime)
#ifdef POSIX
import           System.Directory (doesFileExist)
#endif
import           System.Directory (listDirectory, removeFile)
import           System.FilePath ((</>), splitExtension, takeBaseName,
                     takeDirectory, takeExtension)
import           System.IO (BufferMode (LineBuffering), Handle,
                     IOMode (AppendMode, WriteMode), hFileSize, hSetBuffering,
                     openFile, stdout)

#ifdef POSIX
import           System.Directory (createFileLink)
import           System.FilePath (takeFileName)
#endif

import           Cardano.BM.Data.Rotation (RotationParameters (..))

\end{code}

\subsubsection{Format of a timestamp to be appended to the name of a file.}
\begin{code}
tsformat :: String
tsformat = "%Y%m%d%H%M%S"

\end{code}

\subsubsection{Add current time to name of log file.}\label{code:nameLogFile}\index{nameLogFile}
\begin{code}
nameLogFile :: FilePath -> IO FilePath
nameLogFile filename = do
    let (fstem, fext) = splitExtension filename
    now <- getCurrentTime
    let tsnow = formatTime defaultTimeLocale tsformat now
    return $ fstem ++ "-" ++ tsnow ++ fext

\end{code}

\subsubsection{Open a new log file.}\label{code:evalRotator}\index{evalRotator}
\begin{code}
evalRotator :: RotationParameters -> FilePath -> IO (Handle, Integer, UTCTime)
evalRotator rotation filename = do
    let maxAge  = toInteger $ rpMaxAgeHours   rotation
        maxSize = toInteger $ rpLogLimitBytes rotation

    -- open new log file
    fpath <- nameLogFile filename
    hdl <- catchIO (openFile fpath WriteMode) $
               \e -> do
                   prtoutException ("rot: error while opening log: " ++ fpath) e
                   return stdout    -- fallback to standard output in case of exception
    hSetBuffering hdl LineBuffering

#ifdef POSIX
    -- restrict symbolic links only for unix-like OS
    let symLinkPath = filename
    let logfilePath = takeFileName fpath
    -- delete a symlink if it already exists and create a new
    -- one that points to the correct file.
    symLinkExists <- doesFileExist symLinkPath
    when symLinkExists $
      (removeFile symLinkPath)
        `catchIO` (prtoutException ("cannot remove symlink: " ++ symLinkPath))
    (createFileLink logfilePath symLinkPath)
        `catchIO` (prtoutException ("cannot create symlink: " ++ symLinkPath))
#endif

    -- compute next rotation time
    now <- getCurrentTime
    let rottime = addUTCTime (fromInteger $ maxAge * 3600) now

    return (hdl, maxSize, rottime)

\end{code}

\subsubsection{List log files in dir which match with the given filename ignoring date.}\label{code:listLogFiles}\index{listLogFiles}
\begin{code}
listLogFiles :: FilePath -> IO (Maybe (NonEmpty FilePath))
listLogFiles file = do
    -- find files in the same directory which begin with
    -- the same name
    let directoryPath = takeDirectory file

    files <- listDirectory directoryPath
    return $ NE.nonEmpty $ map (directoryPath </> ) $ sort $ filter fpredicate files
  where
    tslen = 14  -- length of a timestamp
    filename = takeWhile (/= '-') $ takeBaseName file  -- only stem of filename
    fext = takeExtension file  -- only file extension
    fplen = length filename
    fxlen = length fext
    fpredicate path = take fplen path == filename
                      && take 1 (drop fplen path) == "-"
                      && take fxlen (drop (fplen + tslen + 1) path) == fext

\end{code}

\subsubsection{Latest log file which matches filename.}\label{code:latestLogFile}\index{latestLogFile}
\begin{code}
latestLogFile :: FilePath -> IO (Maybe FilePath)
latestLogFile filename =
    listLogFiles filename >>= \fs -> return $ latestLogFile' fs
  where
    latestLogFile' :: Maybe (NonEmpty FilePath) -> Maybe FilePath
    latestLogFile' Nothing      = Nothing
    latestLogFile' (Just flist) = Just $ NE.last flist

\end{code}

\subsubsection{Initialize log file at startup; may append to existing file.}\label{code:initializeRotator}\index{initializeRotator}
\begin{code}
initializeRotator :: RotationParameters -> FilePath -> IO (Handle, Integer, UTCTime)
initializeRotator rotation filename = do
    let maxAge  = toInteger $ rpMaxAgeHours   rotation
        maxSize = toInteger $ rpLogLimitBytes rotation

    latest <- latestLogFile filename
    case latest of
        Nothing -> -- no file to append, return new
            evalRotator rotation filename
        Just fname -> do
            -- check date
            now <- getCurrentTime
            tsfp <- parseTimeM True defaultTimeLocale tsformat (timestamp fname)
            let age = round $ diffUTCTime now tsfp
            if age > (3600 * maxAge)
               then do  -- file is too old, return new
                  evalRotator rotation filename
               else do
                  hdl <- catchIO (openFile fname AppendMode) $
                             \e -> do
                                 prtoutException fname e
                                 return stdout    -- fallback to standard output in case of exception
                  hSetBuffering hdl LineBuffering
                  cursize <- hFileSize hdl
                  let rotationTime = addUTCTime (fromInteger $ maxAge * 3600) tsfp
                  return (hdl, (maxSize - cursize), rotationTime)
  where
    tslen = 14  -- length of timestamp
    timestamp fname = take tslen $ tail $ dropWhile (/= '-') $ takeBaseName fname

\end{code}

\subsubsection{Remove old files; count them and only keep n (from config).}\label{code:cleanupRotator}\index{cleanupRotator}
\begin{code}
cleanupRotator :: RotationParameters -> FilePath -> IO ()
cleanupRotator rotation filename = do
    let keepN0 = fromIntegral (rpKeepFilesNum rotation) :: Int
        keepN = max 1 $ min keepN0 99
    listLogFiles filename >>= removeOldFiles keepN
  where
    removeOldFiles :: Int -> Maybe (NonEmpty FilePath) -> IO ()
    removeOldFiles _ Nothing = return ()
    removeOldFiles n (Just flist) =
        mapM_ removeFile $ reverse $ NE.drop n $ NE.reverse flist

\end{code}

\subsubsection{Display message and stack trace of exception on stdout.}\label{code:prtoutException}\index{prtoutException}
\begin{code}
prtoutException :: Exception e => String -> e -> IO ()
prtoutException msg e = do
    putStrLn msg
    putStrLn ("exception: " ++ displayException e)

\end{code}
