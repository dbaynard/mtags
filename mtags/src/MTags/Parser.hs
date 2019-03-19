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
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ViewPatterns               #-}

module MTags.Parser
  ( Commonmark
  , readCommonmark
  , Tree
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
import           "rio" RIO.Seq                             (Seq, pattern (:|>), pattern Empty)

tagsFromCmark :: HeadingTag tag -> Commonmark -> Seq tag
tagsFromCmark f = tagsFromNode f . commonmarkToNode [] . coerce

newtype Commonmark = Commonmark Text
  deriving newtype (Show, Eq, IsString)

readCommonmark :: FilePath -> IO Commonmark
readCommonmark = fmap Commonmark . readFileUtf8

--------------------------------------------------
-- ** Section tree
--------------------------------------------------

newtype Tree a = Tree (forall r . (a -> r) -> (a -> Seq (Tree a) -> r) -> r)

tree :: (a -> r) -> (a -> Seq (Tree a) -> r) -> Tree a -> r
tree l b (Tree t) = t l b

viewTree :: forall a . Display a => Tree a -> Utf8Builder
viewTree = tree l b
  where
    l :: a -> Utf8Builder
    l = display
    b :: a -> Seq (Tree a) -> Utf8Builder
    b a s =  mconcat
      [ display a
      , "("
      , foldMap (tree l b) s
      , ")"
      ]

leaf :: a -> Tree a
leaf a = Tree $ \l _ -> l a

branch :: a -> Seq (Tree a) -> Tree a
branch a s = Tree $ \l b -> b a s

eg :: Tree Int
eg = branch 0 [branch 1 [leaf 2], leaf 3, branch 4 [branch 5 [leaf 6, leaf 7]]]

appendChild :: forall a . Show a => a -> Tree a -> Tree a
appendChild c = tree l b
  where
    l :: a -> Tree a
    l a =  branch a [leaf c]
    b :: a -> Seq (Tree a) -> Tree a
    b a (s :|> t) = branch a (s :|> tree l b t)
    -- This should not occur, but it might!
    b a Empty = branch a [leaf c]

-- data Tree a
--   = Leaf
--   | Branch a (Seq (Tree a))
-- 
-- -- [1,2,3,3,4,3,4,5,2,3,3,2,3,4,1,2,3,2,3,4,5,2,4,5]
-- 
-- treeFromSeq :: Seq a -> Tree a
-- treeFromSeq getLevel = fix \(tree, se) (cur :<| s@(next :<| _)) = case comparing getLevel cur next of
--   -- cur < next => cur branch, next child
--   LT -> Branch cur $ tree s
--   -- cur == next => cur leaf, next sibling
--   EQ -> se (Leaf a) s
--   -- cur > next => cur leaf, next depends
--   GT -> Leaf cur

--------------------------------------------------
-- * Filtering nodes
--------------------------------------------------

type HeadingTag tag = HeadingLevel -> LineNo -> Text -> tag

tagsFromNode :: HeadingTag tag -> Node -> Seq tag
tagsFromNode f = fix $ \go -> \case
  (Document ns)   -> foldMap go ns
  (Heading n p t) -> [f n p t]
  _               -> []

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
