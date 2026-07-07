
\subsection{Cardano.BM.Plugin}
\label{code:Cardano.BM.Plugin}

%if style == newcode
\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}
module Cardano.BM.Plugin
  ( Plugin (..)
  , loadPlugin
  )
  where

import           System.Remote.Monitoring.Wai (Server)

import           Cardano.BM.Backend.Log (Scribe)
import           Cardano.BM.Backend.Switchboard (Switchboard,
                     addExternalBackend, addExternalScribe, setSbEKGServer)
import           Cardano.BM.Data.Backend
import           Cardano.BM.Data.BackendKind ()
import           Cardano.BM.Data.Output

\end{code}
%endif

\subsubsection{Plugins extend functionality}\label{code:Plugin}\index{Plugin}
A |Plugin| has a name and is either a |Backend| or a |Scribe|.
\begin{code}

data Plugin a = BackendPlugin !(Backend a) BackendKind
              | ScribePlugin Scribe ScribeId
              | EKGPlugin !(Backend a) BackendKind (Maybe Server)
\end{code}

\subsubsection{Plugin behaviour}

\subsubsection{Integrating plugins}
\begin{code}
loadPlugin :: Switchboard a -> Plugin a -> IO ()
loadPlugin sb (BackendPlugin be bk) = do
    addExternalBackend sb be bk
loadPlugin sb (ScribePlugin sc nm) = do
  addExternalScribe sb sc nm
loadPlugin sb (EKGPlugin be bk condSv) = do
    setSbEKGServer condSv sb
    addExternalBackend sb be bk

\end{code}
