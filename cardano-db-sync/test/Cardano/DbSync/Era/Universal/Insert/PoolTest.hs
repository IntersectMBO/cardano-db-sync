{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Era.Universal.Insert.PoolTest (tests) where

import Cardano.Base.IP (IPv4, IPv6, toIPv4, toIPv6)
import qualified Cardano.Db as DB
import Cardano.DbSync.Era.Universal.Insert.Pool (mkPoolRelay)
import Cardano.Ledger.BaseTypes (Port (..), StrictMaybe (..))
import qualified Cardano.Ledger.State as PoolP
import Cardano.Prelude
import qualified Data.Text as Text
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Prelude ()

tests :: IO Bool
tests =
  checkParallel $
    Group
      "Cardano.DbSync.Era.Universal.Insert.Pool"
      [ ("a relay address reaches the database bare", prop_relay_addresses_are_bare)
      , ("no relay address is ever quoted", prop_relay_addresses_are_unquoted)
      ]

-- | Regression coverage for issue #2182. 'Show' on the ledger's address wrappers is
-- @show (show ip)@, so an unwrapped relay used to be stored as @"1.2.3.4"@, quotes
-- included.
prop_relay_addresses_are_bare :: Property
prop_relay_addresses_are_bare =
  withTests 1 . property $
    mkPoolRelay updateId (relayOf (toIPv4 [1, 2, 3, 4]) (toIPv6 [0x2001, 0xdb8, 0, 0, 0, 0, 0, 1]))
      === DB.PoolRelay
        { DB.poolRelayUpdateId = updateId
        , DB.poolRelayIpv4 = Just "1.2.3.4"
        , DB.poolRelayIpv6 = Just "2001:db8::1"
        , DB.poolRelayDnsName = Nothing
        , DB.poolRelayDnsSrvName = Nothing
        , DB.poolRelayPort = Just 41950
        }

prop_relay_addresses_are_unquoted :: Property
prop_relay_addresses_are_unquoted = property $ do
  ipv4 <- forAll $ toIPv4 <$> replicateM 4 (Gen.int (Range.constant 0 255))
  ipv6 <- forAll $ toIPv6 <$> replicateM 8 (Gen.int (Range.constant 0 0xffff))
  let relay = mkPoolRelay updateId (relayOf ipv4 ipv6)
      addrs = catMaybes [DB.poolRelayIpv4 relay, DB.poolRelayIpv6 relay]
  filter (Text.isInfixOf "\"") addrs === []

relayOf :: IPv4 -> IPv6 -> PoolP.StakePoolRelay
relayOf ipv4 ipv6 = PoolP.SingleHostAddr (SJust (Port 41950)) (SJust ipv4) (SJust ipv6)

updateId :: DB.PoolUpdateId
updateId = DB.PoolUpdateId 1
