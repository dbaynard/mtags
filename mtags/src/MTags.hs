-- |
-- Module      : MTags
-- Description : MTags
-- Copyright   : David Baynard 2019
--
-- License     : BSD-3-Clause OR Apache-2.0
-- Maintainer  : David Baynard <haskell@baynard.me>
-- Stability   : experimental
-- Portability : GHC
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

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UndecidableInstances       #-}

module MTags
  ( module MTags
  ) where

import           "base" Data.Coerce                                    (coerce)
import           "generic-lens" Data.Generics.Product
import           "base" Data.List.NonEmpty                             (NonEmpty (..))
import           "prettyprinter" Data.Text.Prettyprint.Doc
import           "prettyprinter" Data.Text.Prettyprint.Doc.Render.Text (renderIO, renderLazy)
import           "validity" Data.Validity
import           "validity-containers" Data.Validity.Sequence          ()
import           "validity-containers" Data.Validity.Set               ()
import           "validity-text" Data.Validity.Text                    ()
import qualified "base" GHC.Exts                                       as GHC (IsList)
import           "base" GHC.TypeLits                                   (Symbol)
import           "this" MTags.Parser
import           "rio" RIO
import           "rio" RIO.FilePath                                    (takeFileName)
import qualified "rio" RIO.List                                        as L (any)
import           "rio" RIO.Seq                                         (Seq)
import qualified "rio" RIO.Seq                                         as Seq (unstableSort)
import qualified "rio" RIO.Text                                        as T (any, pack, toCaseFold)
import           "raw-strings-qq" Text.RawString.QQ

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> :set -XOverloadedLists
-- >>> import Data.Text.Prettyprint.Doc.Render.String
-- >>> let renderPretty = Prelude.putStr . renderString . layoutCompact . pretty
-- >>> :{
-- let exampleMTag = MTag
--       { tagName = "Analysis of proteins"
--       , tagFile = "cry.md"
--       , tagAddress = "/^## Analysis of proteins$/"
--       , fields = [Kind "s", Line 197, Section "Methods"]
--       }
-- :}

--------------------------------------------------
-- * MTags files
--------------------------------------------------

-- TODO no tabs in filename
ctags :: FilePath -> IO ()
ctags file = putCompact . makeMTags (TagFile $ takeFileName file) <=< readCommonmark $ file

--------------------------------------------------
-- * Whole files
--------------------------------------------------

newtype MTagsFile = MTagsFile (Seq MTag)
  deriving stock (Generic)
  deriving newtype (Show, Eq, GHC.IsList)
  deriving anyclass (Validity)

instance Pretty MTagsFile where
  pretty (MTagsFile ms) = vcat $
    [ "!_TAG_FILE_FORMAT\t2"
    , "!_TAG_FILE_SORTED\t1"
    , "!_TAG_FILE_ENCODING\tutf-8"
    ] <> foldMap (pure . pretty) (Seq.unstableSort ms)

makeMTags :: TagFile -> Commonmark -> MTagsFile
makeMTags file = MTagsFile . tagsFromCmark (mtagFromNodes file)

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
  deriving anyclass (Validity)

instance Ord MTag where
  compare = comparing tagName

-- | >>> renderPretty exampleMTag
-- Analysis of proteins cry.md  /^## Analysis of proteins$/;" s line:197  section:Methods
instance Pretty MTag where
  pretty m = mconcat
    [ m ^. typed @TagName . to pretty
    , tab
    , m ^. typed @TagFile . to pretty
    , tab
    , m ^. typed @TagAddress . to pretty
    , ";\""
    , m ^. typed @TagFields . to pretty
    ]

mtagFromNodes :: TagFile -> ConvertTag MTag
mtagFromNodes file el l (t :| ts) = MTag
  { tagName    = TagName t
  , tagFile    = file
  , tagAddress = addressFromElement t el
  , fields     = mconcat
    [ [ Kind $ kindFromElement el ]
    , [ Line l ]
    , case reverse ts of
      []     -> []
      (n:ns) -> [Parent . ParentNames . fmap TagName $ n :| ns]
    ]
  }

--------------------------------------------------
-- ** Elements
--------------------------------------------------

addressFromElement :: Text -> Element -> TagAddress
addressFromElement t = TagAddress . mconcat . f
  where

    f :: Element -> [Text]
    f (Heading h) =
      [ "/^"
      , T.pack . replicate (fromIntegral h) $ '#'
      , " "
      , t
      , "$/"
      ]

    f Figure =
      [ [r|/%({#|=")fig:|]
      , t
      , "/"
      ]

    f Table = [ "/^Table: /" ]

-- | When using with tagbar, set up the kinds as described by 'Element'.
kindFromElement :: Element -> TagKind
kindFromElement (Heading _) = "s"
kindFromElement Figure      = "i"
kindFromElement Table       = "t"

--------------------------------------------------
-- ** Possible field values
--------------------------------------------------

data FieldValue
  = Kind TagKind
  | Line LineNo
  | Parent ParentNames
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Validity)

instance Pretty FieldValue where
  pretty (Kind t) = pretty t

  pretty (Line n)    = mconcat ["line", colon, pretty n]
  pretty (Parent n) = mconcat ["section", colon, pretty n]

  prettyList = foldMap $ (tab <>) . pretty

--------------------------------------------------
-- ** Newtypes
--------------------------------------------------

newtype TagName = TagName Text
  deriving stock (Generic)
  deriving newtype (Show, Eq, IsString, Pretty)
  deriving Validity via (NoChar "\t" Text)
  deriving Ord via CaseFold

newtype TagFile = TagFile FilePath
  deriving stock (Generic)
  deriving newtype (Show, Eq, IsString, Pretty)
  deriving Validity via (NoChar "\t" String)

-- TODO need to apply tag-security restrictions
-- TODO E.g. need to escape backslashes
newtype TagAddress = TagAddress Text
  deriving stock (Generic)
  deriving newtype (Show, Eq, IsString, Pretty, Validity)

-- TODO May not contain special characters.
newtype TagFields = TagFields (Set FieldValue)
  deriving stock (Generic)
  deriving newtype (Show, Eq, Validity, GHC.IsList, Semigroup, Monoid)

instance Pretty TagFields where
  pretty (TagFields fieldSet) = prettyList . toList $ fieldSet

newtype TagKind = TagKind Text
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord, IsString, Pretty)
  deriving Validity via (NoChar ":" Text)

newtype ParentNames = ParentNames (NonEmpty TagName)
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord, Validity)

instance Pretty ParentNames where
  pretty (ParentNames names) = intercalate "|" . fmap pretty . toList $ names

--------------------------------------------------
-- * Helpers
--------------------------------------------------

onJust :: b -> Maybe a -> (a -> b) -> b
onJust = flip . maybe
infixl 6 `onJust`

--------------------------------------------------
-- ** PrettyPrinting
--------------------------------------------------

tab :: Doc ann
tab = "\t"

intercalate :: Doc ann -> [Doc ann] -> Doc ann
intercalate d = concatWith $ \a b -> a <> d <> b

renderCompact :: Pretty a => a -> Utf8Builder
renderCompact = display . renderLazy . layoutCompact . pretty

putCompact :: Pretty a => a -> IO ()
putCompact = renderIO stdout . layoutCompact . pretty

--------------------------------------------------
-- ** Validity
--------------------------------------------------

charNotIn :: Char -> Text -> Bool
charNotIn c = not . T.any (== c)

charNotInS :: Char -> String -> Bool
charNotInS c = not . L.any (== c)

newtype NoChar (s :: Symbol) a = NoChar a

instance Validity (NoChar "\t" Text) where
  validate = declare "Contains no tab character" . charNotIn '\t' . coerce

instance Validity (NoChar "\t" String) where
  validate = declare "Contains no tab character" . charNotInS '\t' . coerce

instance Validity (NoChar ":" Text) where
  validate = declare "Contains no tab character" . charNotIn ':' . coerce

--------------------------------------------------
-- ** Deriving
--------------------------------------------------

newtype CaseFold = CaseFold Text
  deriving Eq

instance Ord CaseFold where
  compare = comparing $ T.toCaseFold . coerce

newtype Field typ a = Field a
  deriving Eq

instance (Eq a, Ord typ, HasType typ a) => Ord (Field typ a) where
  compare = comparing $ view @a (typed @typ) . coerce
