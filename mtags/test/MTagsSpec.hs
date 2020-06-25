{-# LANGUAGE TypeApplications #-}

module MTagsSpec (main, spec) where

import           MTags
import           Test.Hspec
import           Test.Validity
import           TestOrphans ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  xdescribe "MTags" $ do
    genValidSpec @MTag
