{-# LANGUAGE ScopedTypeVariables #-}
-- | Simple test class for Lib. Not much to test, since it's all IO.
module LibSpec where

import Test.Hspec
import Lib

spec :: Spec
spec =
  describe "getEnding" $ do
    it "gives the ending of a file" $
        getEnding "test.txt" `shouldBe` "txt"
    it "gives the ending, even of a filepath" $
        getEnding "file/path/readme.file" `shouldBe` "file"
    it "only gives you the last part after the last dot" $
        getEnding "test.value/file.md" `shouldBe` "md"
  describe "getFileName" $ do
    it "gives you the last part of a path" $
        getFileName "test/last/value" `shouldBe` "value"
    it "doesn't affect normal files" $
        getFileName "file.txt" `shouldBe` "file.txt"
  describe "filterFileTyp" $ do
    it "only returns the files with the specified ending" $
        filterFileTyp "txt" ["test.txt", "a.png", "ball.txt"] `shouldBe` ["test.txt", "ball.txt"]
    it "can return an empty list" $
        filterFileTyp "nothing" [] `shouldBe` []
