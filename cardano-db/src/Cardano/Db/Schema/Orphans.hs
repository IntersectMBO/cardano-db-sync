{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Db.Schema.Orphans where

import           Cardano.Db.Types (DbWord64 (..))

import qualified Data.Char as Char
import           Data.Ratio (denominator, numerator)
import           Data.WideWord.Word128 (Word128)

import qualified Data.Text as Text

import           Database.Persist.Class (PersistField (..))
import           Database.Persist.Sql (PersistFieldSql (..), SqlType (..))
import           Database.Persist.Types (PersistValue (..))

import           Numeric.Natural (Natural)

import           Shelley.Spec.Ledger.PParams (ProtVer (..))

import           Text.ParserCombinators.ReadP (ReadP, char, choice, many1, munch1,
                    readP_to_S, satisfy, string, skipSpaces)
import           Text.Read (readMaybe)

instance PersistField DbWord64 where
  toPersistValue = PersistText . Text.pack . show . unDbWord64
  fromPersistValue (PersistText bs) = Right $ DbWord64 (read $ Text.unpack bs)
  fromPersistValue x =
    Left $ mconcat [ "Failed to parse Haskell type Word64: ", Text.pack (show x) ]

instance PersistField Word128 where
  toPersistValue = PersistText . Text.pack . show
  fromPersistValue (PersistText bs) = Right $ read (Text.unpack bs)
  fromPersistValue x@(PersistRational r) =
    if denominator r == 1
      then Right $ fromIntegral (numerator r)
      else Left $ mconcat [ "Failed to parse Haskell type Word128: ", Text.pack (show x) ]
  fromPersistValue x =
    Left $ mconcat [ "Failed to parse Haskell type Word128: ", Text.pack (show x) ]

instance PersistField ProtVer where
  toPersistValue = PersistText . Text.pack . showProtVer
  fromPersistValue (PersistText txt) =
    case readMaybe (Text.unpack txt) of
      Nothing -> Left $ mconcat [ "Failed to parse Haskell type ProtVer: ", txt ]
      Just pv -> Right pv
  fromPersistValue x =
    Left $ mconcat [ "Failed to parse Haskell type ProtVer: '", Text.pack (show x), "'." ]

instance PersistFieldSql ProtVer where
  sqlType _ = SqlString

-- -----------------------------------------------------------------------------
-- Projectile vomit here

showProtVer :: ProtVer -> String
showProtVer (ProtVer vmaj vmin) =
  "ProtVer " ++ show vmaj ++ ' ' : show vmin

instance Read ProtVer where
  readsPrec = const (readP_to_S readProtVer)

readProtVer :: ReadP ProtVer
readProtVer =
  skipSpaces *>
    choice
      [ char '(' *> readProtVer <* skipSpaces <* char ')'
      , ProtVer
          <$> (string "ProtVer" *> munch1 Char.isSpace *> readPNatural)
          <*> (munch1 Char.isSpace *> readPNatural)
      ]

readPNatural :: ReadP Natural
readPNatural = read <$> many1 (satisfy Char.isDigit)
