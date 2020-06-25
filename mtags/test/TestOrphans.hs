{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestOrphans () where

import           Data.GenValidity.Set  ()
import           Data.GenValidity.Text ()
import           MTags
import           MTags.Parser
import           Test.Validity

instance GenUnchecked MTag
instance GenValid MTag

instance GenUnchecked TagName
instance GenValid TagName

instance GenUnchecked TagFile
instance GenValid TagFile

instance GenUnchecked TagAddress
instance GenValid TagAddress

instance GenUnchecked TagFields
instance GenValid TagFields

instance GenUnchecked FieldValue
instance GenValid FieldValue

instance GenUnchecked TagKind
instance GenValid TagKind

instance GenUnchecked LineNo
instance GenValid LineNo

instance GenUnchecked ParentNames
instance GenValid ParentNames
