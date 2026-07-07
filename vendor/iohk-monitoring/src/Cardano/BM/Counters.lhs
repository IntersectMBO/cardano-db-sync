
\subsection{Cardano.BM.Counters}
\label{code:Cardano.BM.Counters}

The platform is chosen on which we compile this library.

Currently, we mainly support |Linux| with its 'proc' filesystem,
but also partially support |Windows|.

\begin{code}
{-# LANGUAGE CPP #-}

module Cardano.BM.Counters
    (
      Platform.readCounters
    , getMonoClock
    ) where

#if defined(linux_HOST_OS)
import qualified Cardano.BM.Counters.Linux as Platform
#elif defined(mingw32_HOST_OS)
import qualified Cardano.BM.Counters.Windows as Platform
#elif defined(darwin_HOST_OS)
import qualified Cardano.BM.Counters.Darwin as Platform
#else
import qualified Cardano.BM.Counters.Dummy as Platform
#endif

import           Cardano.BM.Counters.Common (getMonoClock)

\end{code}
