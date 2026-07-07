
\subsection{Cardano.BM.Data.Backend}
\label{code:Cardano.BM.Data.Backend}

%if style == newcode
\begin{code}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module Cardano.BM.Data.Backend
  ( Backend (..)
  , BackendKind (..)
  , BackendId
  , IsBackend (..)
  , IsEffectuator (..)
  , GenericBackendFailure (..)
  )
  where

import           Control.Exception (Exception)
import           Data.Aeson (FromJSON)
import           Data.Kind (Type)
import           Data.Text (Text)

import           Cardano.BM.Data.BackendKind
import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Trace
import           Cardano.BM.Configuration (Configuration)

\end{code}
%endif

\subsubsection{BackendId}\label{code:BackendId}\index{BackendId}
A backend is identified by |BackendKind x Name|
\begin{code}
type BackendId = Text

\end{code}

\subsubsection{Accepts a |LogObject|}\label{code:IsEffectuator}\index{IsEffectuator}
Instances of this type class accept a |LogObject| and deal with it.
\begin{code}
class IsEffectuator t a where
    effectuate     :: t a -> LogObject a -> IO ()
    effectuatefrom :: forall s . (IsEffectuator s a) => t a -> LogObject a -> s a -> IO ()
    default effectuatefrom :: forall s . (IsEffectuator s a) => t a -> LogObject a -> s a -> IO ()
    effectuatefrom t nli _ = effectuate t nli
    handleOverflow :: t a -> IO ()

\end{code}

\subsubsection{Declaration of a |Backend|}\label{code:IsBackend}\index{IsBackend}
A backend is life-cycle managed, thus can be |realize|d and |unrealize|d.
\begin{code}
class ( IsEffectuator t a
      , FromJSON a
      , Exception (BackendFailure t)
      ) => IsBackend t a where
    type BackendFailure t :: Type
    type BackendFailure t = GenericBackendFailure
    bekind      :: t a -> BackendKind
    realize     :: Configuration -> IO (t a)
    realizefrom :: forall s . (IsEffectuator s a) => Configuration -> Trace IO a -> s a -> IO (t a)
    default realizefrom :: forall s . (IsEffectuator s a) => Configuration -> Trace IO a -> s a -> IO (t a)
    realizefrom cfg _ _ = realize cfg
    unrealize   :: t a -> IO ()

\end{code}

\subsubsection{Backend}\label{code:Backend}\index{Backend}
This data structure for a backend defines its behaviour
as an |IsEffectuator| when processing an incoming message,
and as an |IsBackend| for unrealizing the backend.
\begin{code}
data Backend a = MkBackend
    { bEffectuate :: LogObject a -> IO ()
    , bUnrealize  :: IO ()
    }

\end{code}

\subsubsection{GenericBackendFailure}\label{code:GenericBackendFailure}\index{GenericBackendFailure}
A default type for backend-specific failures, when they
wouldn't care to define their own.
\begin{code}
newtype GenericBackendFailure =
  GenericBackendFailure { unGenericBackendFailure :: String }

instance Exception GenericBackendFailure
instance Show GenericBackendFailure where
  show x = "Generic backend failure: " <> unGenericBackendFailure x

\end{code}
