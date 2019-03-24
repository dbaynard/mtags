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
  , ConvertTag
  , tagsFromCmark
  , tagsFromNode
  , HeadingLevel
  , LineNo
  ) where

import           "cmark" CMark
import           "base" Data.Coerce                        (coerce)
import           "base" Data.List.NonEmpty                 (NonEmpty)
import qualified "base" Data.List.NonEmpty                 as NE (fromList)
import           "prettyprinter" Data.Text.Prettyprint.Doc
import           "validity" Data.Validity
import           "rio" RIO
import           "rio" RIO.Seq                             (Seq ((:|>)))
import qualified "rio" RIO.Seq                             as Seq (take)
import qualified "rio" RIO.Text                            as T (stripPrefix, takeWhile)

tagsFromCmark :: ConvertTag tag -> Commonmark -> Seq tag
tagsFromCmark f = tagsFromNode f . commonmarkToNode [] . coerce

newtype Commonmark = Commonmark Text
  deriving newtype (Show, Eq, IsString)

readCommonmark :: FilePath -> IO Commonmark
readCommonmark = fmap Commonmark . readFileUtf8

--------------------------------------------------
-- * Filtering nodes
--------------------------------------------------

type ConvertTag tag = Maybe HeadingLevel -> LineNo -> NonEmpty Text -> tag

-- TODO Non empty Seq!
tagsFromNode :: forall tag . ConvertTag tag -> Node -> Seq tag
tagsFromNode f = fst . g []
  where
    g :: Seq Text -> Node -> (Seq tag, Seq Text)
    g = fix $ \go stack -> \case
      (Document ns)    -> foldM go [] ns
      -- This case shouldn't happen
      (Heading 0 l t) -> ([f (Just 1) l $ [t]], [t])
      (Heading n l t) -> let nstack = Seq.take (fromIntegral n - 1) stack :|> t in
        ([f (Just n) l . report $ nstack], nstack)
      (FigureDiv l t)  -> ([f Nothing l . report $ stack :|> t], stack)
      (Figure    l t)  -> ([f Nothing l . report $ stack :|> t], stack)
      _                -> ([], stack)
    report :: Seq Text -> NonEmpty Text
    report = NE.fromList . reverse . toList

pattern Document :: [Node] -> Node
pattern Document ns <- Node _ DOCUMENT ns

pattern Heading :: HeadingLevel -> LineNo -> Text -> Node
pattern Heading n l t <- Node
  (Just PosInfo{startLine = fromIntegral -> l})
  (HEADING (fromIntegral -> n))
  [Node Nothing (TEXT t) []]

identOnly :: Text -> Text
identOnly = T.takeWhile (\x -> x /= ' ' && x /= '}')

pattern FigureDiv :: LineNo -> Text -> Node
pattern FigureDiv l t <- Node
  (Just PosInfo{startLine = fromIntegral -> l})
  PARAGRAPH
  (Node Nothing (TEXT (fmap identOnly . T.stripPrefix "::: {#fig:" -> Just t)) [] : _)

pattern Figure :: LineNo -> Text -> Node
pattern Figure l t <- Node
  (Just PosInfo{startLine = fromIntegral -> l})
  PARAGRAPH
  [ Node Nothing (IMAGE _ _) _
  , Node Nothing (TEXT (fmap identOnly . T.stripPrefix "{#fig:" -> Just t)) []
  ]

newtype HeadingLevel = HeadingLevel Word
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord, Enum, Bounded, Num, Real, Integral)
  deriving anyclass (Validity)

newtype LineNo = LineNo Word
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord, Enum, Bounded, Num, Real, Integral, Pretty)
  deriving anyclass (Validity)
