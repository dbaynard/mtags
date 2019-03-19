-- |
-- Module      : MTags.Parser
-- Description : Parse commonmark files to get tags
-- Copyright   : David Baynard 2019
--
-- License     : BSD-3-Clause OR Apache-2.0
-- Maintainer  : David Baynard <haskell@baynard.me>
-- Stability   : experimental
-- Portability : unknown

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE PatternSynonyms   #-}

module MTags.Parser
  ( tagFromNode
  ) where

import           "cmark" CMark
import           "rio" RIO

--------------------------------------------------
-- * Filtering nodes
--------------------------------------------------

tagFromNode :: (Int -> PosInfo -> Text -> Maybe tag) -> Node -> Maybe tag
tagFromNode f (Heading n p t) = f n p t
tagFromNode _ _               = Nothing

pattern Heading :: Int -> PosInfo -> Text -> Node
pattern Heading n p t <- Node (Just p) (HEADING n) [Node Nothing (TEXT t) []]
