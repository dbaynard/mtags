-- |
-- Module      : MTags.Parser
-- Description : Parse commonmark files to get tags
-- Copyright   : David Baynard 2019
--
-- License     : BSD-3-Clause OR Apache-2.0
-- Maintainer  : David Baynard <haskell@baynard.me>
-- Stability   : experimental
-- Portability : unknown

{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}

module MTags.Parser
  ( Commonmark
  , readCommonmark
  , HeadingTag
  , tagsFromCmark
  , tagsFromNode
  , HeadingLevel
  , LineNo
  ) where

import           "cmark" CMark
import           "base" Data.Coerce                        (coerce)
import           "prettyprinter" Data.Text.Prettyprint.Doc
import           "validity" Data.Validity
import           "rio" RIO
import           "rio" RIO.Seq                             (Seq ((:|>)))
import qualified "rio" RIO.Seq                             as Seq (take)
import qualified "rio" RIO.Text                            as T (intercalate)

tagsFromCmark :: HeadingTag tag -> Commonmark -> Seq tag
tagsFromCmark f = tagsFromNode f . commonmarkToNode [] . coerce

newtype Commonmark = Commonmark Text
  deriving newtype (Show, Eq, IsString)

readCommonmark :: FilePath -> IO Commonmark
readCommonmark = fmap Commonmark . readFileUtf8

--------------------------------------------------
-- * Filtering nodes
--------------------------------------------------

type HeadingTag tag = HeadingLevel -> LineNo -> Text -> tag

tagsFromNode :: forall tag . HeadingTag tag -> Node -> Seq tag
tagsFromNode f = fst . go []
  where
    go :: Seq Text -> Node -> (Seq tag, Seq Text)
    go stack = \case
      -- \go stack -> \case
      (Document ns)   -> foldM go [] ns
      -- This case shouldn't happen
      (Heading 0 p t) -> ([f 1 p t], [t])
      (Heading n p t) -> let nstack = Seq.take (fromIntegral n - 1) stack :|> t in
        ([f n p . T.intercalate "|" . toList $ nstack], nstack)
      _               -> ([], stack)

pattern Document :: [Node] -> Node
pattern Document ns <- Node _ DOCUMENT ns

pattern Heading :: HeadingTag Node
pattern Heading n l t <- Node
  (Just PosInfo{startLine = fromIntegral -> l})
  (HEADING (fromIntegral -> n))
  [Node Nothing (TEXT t) []]

newtype HeadingLevel = HeadingLevel Word
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord, Enum, Bounded, Num, Real, Integral)
  deriving anyclass (Validity)

newtype LineNo = LineNo Word
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord, Enum, Bounded, Num, Real, Integral, Pretty)
  deriving anyclass (Validity)
