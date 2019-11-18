{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

-- | Type-level specification of Explorer API (via Servant).

module Explorer.Web.Api.Legacy
       ( ExplorerApiRecord(..)
       , TxsStats
       , PageNumber
       ) where

import           Data.Word                (Word16, Word64)
import           Data.Int (Int64)
import           GHC.Generics             (Generic)
import           Servant.API              ((:>), Capture, FromHttpApiData, Get,
                                           JSON, Post, QueryParam, ReqBody,
                                           Summary)
import           Servant.API.Generic      ((:-))

import           Explorer.DB              (Ada)
import           Cardano.Chain.Slotting   (EpochNumber (EpochNumber))
import           Explorer.Web.ClientTypes (CAddress, CAddressSummary,
                                           CAddressesFilter, CBlockEntry,
                                           CBlockRange, CBlockSummary,
                                           CGenesisAddressInfo, CGenesisSummary,
                                           CHash, CTxBrief, CTxEntry, CTxHash,
                                           CTxSummary, CUtxo)
import           Explorer.Web.Error       (ExplorerError)
import           Explorer.Web.Api.Legacy.Types (PageNo (..), PageSize (..))

type PageNumber = Word

-- | Shortcut for common api result types.
type ExRes verb a = verb '[JSON] (Either ExplorerError a)

-- doing this without an orphan would require adding servant to the deps of cardano-ledger
deriving instance FromHttpApiData EpochNumber

-- | A servant-generic record with all the methods of the API
data ExplorerApiRecord route = ExplorerApiRecord
  {
    _totalAda :: route
        :- "supply"
        :> "ada"
        :> ExRes Get Ada

  , _blocksPages :: route
        :- Summary "Get the list of blocks, contained in pages."
        :> "blocks"
        :> "pages"
        :> QueryParam "page" PageNo
        :> QueryParam "pageSize" PageSize
        :> ExRes Get (PageNumber, [CBlockEntry])

  , _dumpBlockRange :: route
        :- Summary "Dump a range of blocks, including all tx in those blocks"
        :> "blocks"
        :> "range"
        :> Capture "start" CHash
        :> Capture "stop" CHash
        :> ExRes Get CBlockRange

  , _blocksPagesTotal :: route
        :- Summary "Get the list of total pages."
        :> "blocks"
        :> "pages"
        :> "total"
        :> QueryParam "pageSize" PageSize
        :> ExRes Get PageNumber

  , _blocksSummary :: route
        :- Summary "Get block's summary information."
        :> "blocks"
        :> "summary"
        :> Capture "hash" CHash
        :> ExRes Get CBlockSummary

  , _blocksTxs :: route
        :- Summary "Get brief information about transactions."
        :> "blocks"
        :> "txs"
        :> Capture "hash" CHash
        :> QueryParam "limit" Int64
        :> QueryParam "offset" Int64
        :> ExRes Get [CTxBrief]

  , _txsLast :: route
        :- Summary "Get information about the N latest transactions."
        :> "txs"
        :> "last"
        :> ExRes Get [CTxEntry]

  , _txsSummary :: route
        :- Summary "Get summary information about a transaction."
        :> "txs"
        :> "summary"
        :> Capture "txid" CTxHash
        :> ExRes Get CTxSummary

  , _addressSummary :: route
        :- Summary "Get summary information about an address."
        :> "addresses"
        :> "summary"
        :> Capture "address" CAddress
        :> ExRes Get CAddressSummary

  , _addressUtxoBulk :: route
        :- Summary "Get summary information about multiple addresses."
        :> "bulk"
        :> "addresses"
        :> "utxo"
        :> ReqBody '[JSON] [CAddress]
        :> ExRes Post [CUtxo]

  , _epochPages :: route
        :- Summary "Get epoch pages, all the paged slots in the epoch."
        :> "epochs"
        :> Capture "epoch" EpochNumber
        :> QueryParam "page" PageNo
        :> ExRes Get (Int, [CBlockEntry])

  , _epochSlots :: route
        :- Summary "Get the slot information in an epoch."
        :> "epochs"
        :> Capture "epoch" EpochNumber
        :> Capture "slot" Word16
        :> ExRes Get [CBlockEntry]

  , _genesisSummary :: route
        :- "genesis"
        :> "summary"
        :> ExRes Get CGenesisSummary

  , _genesisPagesTotal :: route
        :- "genesis"
        :> "address"
        :> "pages"
        :> "total"
        :> QueryParam "pageSize" PageSize
        :> QueryParam "filter" CAddressesFilter
        :> ExRes Get PageNumber

  , _genesisAddressInfo :: route
        :- "genesis"
        :> "address"
        :> QueryParam "page" PageNo
        :> QueryParam "pageSize" PageSize
        :> QueryParam "filter" CAddressesFilter
        :> ExRes Get [CGenesisAddressInfo]

  , _statsTxs :: route
        :- "stats"
        :> "txs"
        :> QueryParam "page" PageNo
        :> ExRes Get TxsStats

  -- Although this is a new endpoint its return type is a type already used here
  -- in the legacy API.
  , _blockAddress
        :: route
        :- "block"
        :> Capture "block" CHash
        :> "address"
        :> Capture "address" CAddress
        :> Get '[JSON] (Either ExplorerError CAddressSummary)

  }
  deriving (Generic)

type TxsStats = (PageNumber, [(CTxHash, Word64)])
