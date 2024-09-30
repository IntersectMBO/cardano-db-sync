{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Db.Git.Version (
  gitRev,
) where

import Cardano.Db.Git.RevFromGit (gitRevFromGit)
import Data.FileEmbed (dummySpaceWith)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

-- | Provide the git revision at compilation time (if possible)
--
-- Used to provide a git revision in cli programs
gitRev :: Text
gitRev
  | gitRevEmbed /= zeroRev = gitRevEmbed
  | Text.null fromGit = zeroRev
  | otherwise = fromGit
  where
    -- Git revision embedded after compilation using
    -- Data.FileEmbed.injectWith. If nothing has been injected,
    -- this will be filled with 0 characters.
    gitRevEmbed :: Text
    gitRevEmbed = Text.decodeUtf8 $(dummySpaceWith "gitrev" 40)

-- Git revision found during compilation by running git. If
-- git could not be run, then this will be empty.
#if defined(arm_HOST_ARCH)
    -- cross compiling to arm fails; due to a linker bug
    fromGit = ""
#else
    fromGit = Text.strip (Text.pack $(gitRevFromGit))
#endif

zeroRev :: Text
zeroRev = "0000000000000000000000000000000000000000"
