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
import           "rio" RIO.Seq                             (Seq)

tagsFromCmark :: HeadingTag tag -> Commonmark -> Seq tag
tagsFromCmark f = tagsFromNode f . commonmarkToNode [] . coerce

newtype Commonmark = Commonmark Text
  deriving newtype (Show, Eq, IsString)

readCommonmark :: FilePath -> IO Commonmark
readCommonmark = fmap Commonmark . readFileUtf8

--------------------------------------------------
-- ** Section tree
--------------------------------------------------

data Tree' a
  = Leaf'
  | Branch' a (Seq (Tree' a))

viewTree' :: Display a => Tree' a -> Utf8Builder
viewTree' (Leaf') = ""
viewTree' (Branch' a s) = mconcat
  [ display a
  , "("
  , foldMap viewTree' s
  , ")"
  ]

data TreeDict a r = TreeDict
  { leaf' :: r
  , branch' :: a -> Seq r -> r
  }

newtype Tree a = Tree (forall r . r -> (a -> Seq r -> r) -> r)

tree :: r -> (a -> Seq r -> r) -> Tree a -> r
tree l b (Tree t) = t l b

viewTree :: forall a . Display a => Tree a -> Utf8Builder
viewTree = tree l b
  where
    l :: Utf8Builder
    l = ""
    b :: a -> Seq Utf8Builder -> Utf8Builder
    b a s =  mconcat
      [ display a
      , "("
      , fold s
      , ")"
      ]

leaf :: Tree a
leaf = Tree $ \l _ -> l

branch :: a -> Seq (Tree a) -> Tree a
branch a s = Tree $ \l b -> b a (tree l b <$> s)

eg1 :: Tree' Int
eg1 = Branch' 0 [Branch' 1 [Leaf'], Branch' 2 [Branch' 3 []]]

eg2 :: Tree Int
eg2 = branch 0 [branch 1 [leaf], branch 2 [branch 3 []]]

data TreeF a r
  = LeafF
  | BranchF a (Seq r)

roll :: TreeF a (Tree a) -> Tree a
roll LeafF         = leaf
roll (BranchF a s) = branch a s

unroll :: forall a . Tree a -> TreeF a (Tree a)
unroll = tree l b
  where
    l :: TreeF a (Tree a)
    l     = LeafF
    b :: a -> Seq (TreeF a (Tree a)) -> TreeF a (Tree a)
    b a s = BranchF a (roll <$> s)

newtype TreeD a = TreeD (forall w . w -> (a -> Seq (Tree a) -> w) -> w)

treeD :: w -> (a -> Seq (Tree a) -> w) -> TreeD a -> w
treeD l b (TreeD t) = t l b

decon :: Tree a -> TreeD a
decon = tree l b
  where
    l :: TreeD a
    l = TreeD $ \lD _ -> lD
    b :: a -> Seq (TreeD a) -> TreeD a
    b a s = TreeD $ \lD bD -> bD a (treeD leaf branch <$> s)

viewTreeD :: forall a . Display a => Tree a -> Utf8Builder
viewTreeD = treeD l b . decon
  where
    l :: Utf8Builder
    l = ""
    b :: a -> Seq (Tree a) -> Utf8Builder
    b a s =  mconcat
      [ display a
      , "("
      , foldMap viewTreeD s
      , ")"
      ]

-- addChild :: a -> Tree a -> Tree a
-- addChild a t = Tree $ \_ b -> b a _

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
