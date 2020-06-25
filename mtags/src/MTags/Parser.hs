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
  , OutputFile(..)
  , nodeFileFromMd
  , Element(..)
  , ConvertTag
  , tagsFromCmark
  , tagsFromNode
  , HeadingLevel
  , LineNo
  ) where

import           "cmark" CMark
import           "base" Data.Coerce                        (coerce)
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

newtype OutputFile = OutputFile FilePath
  deriving newtype (Show, Eq, IsString)

nodeFileFromMd :: OutputFile -> FilePath -> IO ()
nodeFileFromMd (OutputFile f) =  writeFileUtf8 f . tshow . commonmarkToNode [] . coerce <=< readCommonmark

--------------------------------------------------
-- * Filtering nodes
--------------------------------------------------

-- | In vim, set up the Tagbar @kinds@ array as follows:
--
-- @
-- \\ \'kinds\' : [
--     \\ \'s:sections\',
--     \\ \'i:images\',
--     \\ \'t:tables\'
-- \\ ],
-- @
data Element
  = Heading HeadingLevel
  | Figure
  | Table
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Validity)

type ConvertTag tag = Element -> LineNo -> NonEmpty Text -> tag

-- TODO Non empty Seq!
tagsFromNode :: forall tag . ConvertTag tag -> Node -> Seq tag
tagsFromNode f = fst . g []
  where
    g :: Seq Text -> Node -> (Seq tag, Seq Text)
    g = fix $ \go stack -> \case
      (DocumentN ns)    -> foldM go [] ns
      -- This case shouldn't happen
      (HeadingN 0 l t) -> ([f (Heading 1) l $ [t]], [t])
      (HeadingN n l t) -> let nstack = Seq.take (fromIntegral n - 1) stack :|> t in
        ([f (Heading n) l . report $ nstack], nstack)
      (FigureDivN l t)  -> ([f Figure l . report $ stack :|> t], stack)
      (FigureN    l t)  -> ([f Figure l . report $ stack :|> t], stack)
      (TableN     l t)  -> ([f Table l . report $ stack :|> t], stack)
      _                 -> ([], stack)
    report :: Seq Text -> NonEmpty Text
    report = NE.fromList . reverse . toList

pattern DocumentN :: [Node] -> Node
pattern DocumentN ns <- Node _ DOCUMENT ns

pattern HeadingN :: HeadingLevel -> LineNo -> Text -> Node
pattern HeadingN n l t <- Node
  (Just PosInfo{startLine = fromIntegral -> l})
  (HEADING (fromIntegral -> n))
  [Node Nothing (TEXT t) []]

identOnly :: Text -> Text
identOnly = T.takeWhile (\x -> x /= ' ' && x /= '}' && x /= '\"')

stripFigDiv :: Text -> Maybe Text
stripFigDiv t = identOnly <$> do
  T.stripPrefix "::: {#fig:" t <|> T.stripPrefix "<div id=\"fig:" t

pattern FigureDivN :: LineNo -> Text -> Node
pattern FigureDivN l t <- Node
  (Just PosInfo{startLine = fromIntegral -> l})
  PARAGRAPH
  (Node Nothing (TEXT (stripFigDiv -> Just t)) [] : _)

pattern FigureN :: LineNo -> Text -> Node
pattern FigureN l t <- Node
  (Just PosInfo{startLine = fromIntegral -> l})
  PARAGRAPH
  [ Node Nothing (IMAGE _ _) _
  , Node Nothing (TEXT (fmap identOnly . T.stripPrefix "{#fig:" -> Just t)) []
  ]

pattern TableN :: LineNo -> Text -> Node
pattern TableN l t <- Node
  (Just PosInfo{startLine = fromIntegral -> l})
  PARAGRAPH
  (HeadAndLast
    ( Node Nothing (TEXT (T.stripPrefix "Table:" -> Just _)) _ )
    ( Node Nothing (TEXT (fmap identOnly . T.stripPrefix "{#tbl:" -> Just t)) [] )
  )

pattern HeadAndLast :: a -> a -> [a]
pattern HeadAndLast h l <- h : (reverse -> l:_)

newtype HeadingLevel = HeadingLevel Word
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord, Enum, Bounded, Num, Real, Integral)
  deriving anyclass (Validity)

newtype LineNo = LineNo Word
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord, Enum, Bounded, Num, Real, Integral, Pretty)
  deriving anyclass (Validity)
