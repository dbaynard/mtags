-- |
-- Module      : MTags.Parser
-- Description : Parse commonmark files to get tags
-- Copyright   : David Baynard 2019
--
-- License     : BSD-3-Clause OR Apache-2.0
-- Maintainer  : David Baynard <haskell@baynard.me>
-- Stability   : experimental
-- Portability : unknown

{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE PatternSynonyms            #-}

module MTags.Parser
  ( Commonmark
  , CreateTag
  , tagsFromText
  , tagsFromNode
  ) where

import           "cmark" CMark
import           "base" Data.Coerce (coerce)
import           "rio" RIO
import           "rio" RIO.Seq      (Seq)

tagsFromText :: CreateTag tag -> Commonmark -> Seq tag
tagsFromText f = tagsFromNode f . commonmarkToNode [] . coerce

newtype Commonmark = Commonmark Text
  deriving newtype (Show, Eq, IsString)

--------------------------------------------------
-- * Filtering nodes
--------------------------------------------------

type CreateTag tag = Int -> PosInfo -> Text -> Maybe tag

tagsFromNode :: CreateTag tag -> Node -> Seq tag
tagsFromNode f = fix $ \go -> \case
  (Document ns)   -> foldMap go ns
  (Heading n p t) -> maybe [] pure $ f n p t
  _               -> []

pattern Document :: [Node] -> Node
pattern Document ns <- Node _ DOCUMENT ns

pattern Heading :: Int -> PosInfo -> Text -> Node
pattern Heading n p t <- Node (Just p) (HEADING n) [Node Nothing (TEXT t) []]
