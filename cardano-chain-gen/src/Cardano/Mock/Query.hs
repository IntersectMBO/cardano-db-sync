{-# LANGUAGE TypeApplications #-}

module Cardano.Mock.Query (
  queryVersionMajorFromEpoch,
  queryParamProposalFromEpoch,
) where

import qualified Cardano.Db as Db
import Cardano.Prelude hiding (from)
import Database.Esqueleto.Experimental
import Prelude ()

-- | Query protocol parameters from @EpochParam@ by epoch number. Note that epoch
--   parameters are inserted at the beginning of the next epoch.
--
-- TODO[sgillespie]: It would probably be better to return @Db.EpochParam@, but
-- persistent seems to be having trouble with the data:
--
--     PersistMarshalError "Couldn't parse field `govActionLifetime` from table
--     `epoch_param`. Failed to parse Haskell type `Word64`; expected integer from
--     database, but received: PersistRational (0 % 1).
queryVersionMajorFromEpoch ::
  MonadIO io =>
  Word64 ->
  ReaderT SqlBackend io (Maybe Word16)
queryVersionMajorFromEpoch epochNo = do
  res <- selectOne $ do
    prop <- from $ table @Db.EpochParam
    where_ (prop ^. Db.EpochParamEpochNo ==. val epochNo)
    pure (prop ^. Db.EpochParamProtocolMajor)
  pure $ unValue <$> res

-- | Query protocol parameter proposals from @ParamProposal@ by epoch number.
queryParamProposalFromEpoch ::
  MonadIO io =>
  Word64 ->
  ReaderT SqlBackend io (Maybe Db.ParamProposal)
queryParamProposalFromEpoch epochNo = do
  res <- selectOne $ do
    prop <- from $ table @Db.ParamProposal
    where_ $ prop ^. Db.ParamProposalEpochNo ==. val (Just epochNo)
    pure prop
  pure $ entityVal <$> res
