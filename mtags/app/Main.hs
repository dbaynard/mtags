{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PackageImports     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Main where

import           MTags                              (ctags)
import           "optparse-generic" Options.Generic

data Options w = Options
  ( w ::: FilePath <?> "The input file to scan"
  ) deriving (Generic)

instance ParseRecord (Options Wrapped)
deriving instance Show (Options Unwrapped)

main :: IO ()
main = do
  Options inputFile <- unwrapRecord "ctags replacement for markdown"
  ctags inputFile
