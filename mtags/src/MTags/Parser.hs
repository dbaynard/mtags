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
  , tree
  , leaf
  , branch
  , appendChild
  , HeadingTag
  , tagsFromCmark
  , tagsFromNode
  , HeadingLevel
  , LineNo
  ) where

import           "cmark" CMark
-- import           "base" Data.Coerce                        (coerce)
import           "prettyprinter" Data.Text.Prettyprint.Doc
import           "validity" Data.Validity
import           "rio" RIO
import           "rio" RIO.Seq                             (pattern (:|>), pattern Empty, Seq)

-- tagsFromCmark :: HeadingTag tag -> Commonmark -> Tree tag
-- tagsFromCmark f = tagsFromNode f . commonmarkToNode [] . coerce
tagsFromCmark :: HeadingTag tag -> Commonmark -> Seq tag
tagsFromCmark = undefined

newtype Commonmark = Commonmark Text
  deriving newtype (Show, Eq, IsString)

readCommonmark :: FilePath -> IO Commonmark
readCommonmark = fmap Commonmark . readFileUtf8

--------------------------------------------------
-- ** Section tree
--------------------------------------------------

-- $setup
--
-- eg :: Tree Int
-- eg = branch 0 [branch 1 [leaf 2], leaf 3, branch 4 [branch 5 [leaf 6, leaf 7]]]

-- TODO use NESeq
newtype Tree a = Tree (forall r . (a -> r) -> (a -> Seq (Tree a) -> r) -> r)

tree :: (a -> r) -> (a -> Seq (Tree a) -> r) -> Tree a -> r
tree l b (Tree t) = t l b

leaf :: a -> Tree a
leaf a = Tree $ \l _ -> l a

branch :: a -> Seq (Tree a) -> Tree a
branch a s = Tree $ \_ b -> b a s

instance Show a => Show (Tree a) where
  show = viewTree show

-- | >>> display eg
-- "0(1(2)34(5(67(8))))"
instance Display a => Display (Tree a) where
  display = viewTree display

viewTree :: forall a str . (Monoid str, IsString str) => (a -> str) -> Tree a -> str
viewTree disp = tree l b
  where
    l :: a -> str
    l = disp
    b :: a -> Seq (Tree a) -> str
    b a s =  mconcat
      [ disp a
      , "("
      , foldMap (tree l b) s
      , ")"
      ]

latestLeaf :: forall a . Tree a -> a
latestLeaf = tree id b
  where
    b :: a -> Seq (Tree a) -> a
    b _ (_ :|> t) = tree id b t
    -- This should not occur, but it might!
    b a Empty = a

latestParent :: forall a . Tree a -> Maybe a
latestParent = tree l b
  where
    l :: a -> Maybe a
    l = const Nothing
    b :: a -> Seq (Tree a) -> Maybe a
    b a (s :|> t) = tree pure b t
    -- This should not occur, but it might!
    b _ Empty = Nothing

stripLatestChildren :: forall a . Tree a -> Maybe (Tree a)
stripLatestChildren = tree l b
  where
    l :: a -> Maybe (Tree a)
    l = const Nothing
    b :: a -> Seq (Tree a) -> Maybe (Tree a)
    b a (s :|> t) = tree (const . Just . leaf $ a) b t
    -- This should not occur, but it might!
    b _ Empty = Nothing

appendChild :: forall a . a -> Tree a -> Tree a
appendChild c = tree l b
  where
    l :: a -> Tree a
    l a = branch a [leaf c]
    b :: a -> Seq (Tree a) -> Tree a
    b a (s :|> t) = branch a (s :|> tree l b t)
    -- This should not occur, but it might!
    b a Empty = branch a [leaf c]

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

-- tagsFromNode :: HeadingTag tag -> Node -> Tree tag
-- go :: Node -> Tree tag -> Tree tag
-- tagsFromNode f = \go nd tr -> case nd of
  -- (Document ns)   -> foldr go leaf ns
tagsFromNode :: HeadingTag tag -> Node -> Tree tag -> Tree tag
tagsFromNode f = \nd tr -> case nd of
  (Heading 1 l t) -> f 1 l t `appendChild` tr
  (Heading n l t) ->
    let tag = f n l t
        mup = stripLatestChildren tr
        lst = latestLeaf <$> mup
    in case n - lst of
      0 -> 
      x | x > 0 = 
      x | x < 0 = 
--  (Heading n l t) -> let r = f n l t in case n `compare` 0 of
--    LT -> r
--    EQ -> r
--    GT -> r
  _ -> tr

-- pattern Document :: [Node] -> Node
-- pattern Document ns <- Node _ DOCUMENT ns

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
