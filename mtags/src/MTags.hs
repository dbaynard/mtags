-- |
-- Module      : MTags
-- Description : MTags
-- Copyright   : David Baynard 2019
--
-- License     : BSD-3-Clause OR Apache-2.0
-- Maintainer  : David Baynard <haskell@baynard.me>
-- Stability   : experimental
-- Portability : unknown
--
-- From neovim’s tag-file-format help text:
--
-- @
-- 3.  {tagname}   {TAB} {tagfile} {TAB} {tagaddress} {term} {field} ..
--
-- {tagname}        The identifier.  Normally the name of a function, but it can
--                 be any identifier.  It cannot contain a <Tab>.
-- {TAB}            One <Tab> character.  Note: previous versions allowed any
--                 white space here.  This has been abandoned to allow spaces in
--                 {tagfile}.  It can be re-enabled by including the
--                 |+tag_any_white| feature at compile time. *tag-any-white*
-- {tagfile}        The file that contains the definition of {tagname}.  It can
--                 have an absolute or relative path.  It may contain environment
--                 variables and wildcards (although the use of wildcards is
--                 doubtful).  It cannot contain a <Tab>.
-- {tagaddress}     The Ex command that positions the cursor on the tag.  It can
--                 be any Ex command, although restrictions apply (see
--                 |tag-security|).  Posix only allows line numbers and search
--                 commands, which are mostly used.
-- {term}           ;" The two characters semicolon and double quote.  This is
--                 interpreted by Vi as the start of a comment, which makes the
--                 following be ignored.  This is for backwards compatibility
--                 with Vi, it ignores the following fields.
-- {field} ..        A list of optional fields.  Each field has the form:
--
--                         <Tab>{fieldname}:{value}
--
--                 The {fieldname} identifies the field, and can only contain
--                 alphabetical characters [a-zA-Z].
--                 The {value} is any string, but cannot contain a <Tab>.
--                 These characters are special:
--                         "\t" stands for a <Tab>
--                         "\r" stands for a <CR>
--                         "\n" stands for a <NL>
--                         "\\" stands for a single '\' character
--
--                 There is one field that doesn't have a ":".  This is the kind
--                 of the tag.  It is handled like it was preceded with "kind:".
--                 See the documentation of ctags for the kinds it produces.
--
--                 The only other field currently recognized by Vim is "file:"
--                 (with an empty value).  It is used for a static tag.
-- @

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TypeApplications           #-}

module MTags
  ( module MTags
  ) where

import           "generic-lens" Data.Generics.Product
import           "prettyprinter" Data.Text.Prettyprint.Doc
import           "rio" RIO

--------------------------------------------------
-- * Individual tags
--------------------------------------------------

-- | A single line, corresponding to a single tag.
data MTag = MTag
  { tagName    :: TagName    -- ^ Identifier
  , tagFile    :: TagFile    -- ^ File corresponding to tag
  , tagAddress :: TagAddress -- ^ (Ex) command to locate the tag
  , fields     :: TagFields  -- ^ Optional fields
  }
  deriving stock (Show, Eq, Generic)

instance Pretty MTag where
  pretty m = mconcat
    [ m ^. typed @TagName . to pretty
    , tab
    , m ^. typed @TagFile . to pretty
    , tab
    , m ^. typed @TagAddress . to pretty
    , ";\""
    , m ^. typed @TagAddress . to pretty
    ]

-- TODO need to ensure this contains no tab character
newtype TagName = TagName Text
  deriving newtype (Show, Eq, IsString, Pretty)

-- TODO need to ensure this contains no tab character
newtype TagFile = TagFile FilePath
  deriving newtype (Show, Eq, IsString, Pretty)

-- TODO need to apply tag-security restrictions
newtype TagAddress = TagAddress Text
  deriving newtype (Show, Eq, IsString, Pretty)

-- TODO May not contain tab. Also need to check special characters.
newtype TagFields = TagFields (Set FieldValue)
  deriving newtype (Show, Eq)

instance Pretty TagFields where
  pretty (TagFields fieldSet) = prettyList . toList $ fieldSet

-- TODO ensure that Kind contains no colon
data FieldValue
  = Kind TagKind
  deriving stock (Show, Eq)

instance Pretty FieldValue where
  pretty (Kind t) = pretty t
  -- pretty (Tag t) = mconcat ["tag", colon, pretty t]

  prettyList = foldMap $ (tab <>) . pretty

newtype TagKind = TagKind Text
  deriving newtype (Show, Eq, IsString, Pretty)

--------------------------------------------------
-- * PrettyPrinting
--------------------------------------------------

tab :: Doc ann
tab = "\t"
