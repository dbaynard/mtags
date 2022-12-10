{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestOrphans () where

import Data.GenValidity.Set ()
import Data.GenValidity.Text ()
import MTags
import MTags.Parser
import Test.Validity

instance GenValid MTag

instance GenValid TagName

instance GenValid TagFile

instance GenValid TagAddress

instance GenValid TagFields

instance GenValid FieldValue

instance GenValid TagKind

instance GenValid LineNo

instance GenValid ParentNames
