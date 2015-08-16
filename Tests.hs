module Main where
 
import Hangman
import Test.Hspec
 
main :: IO ()
main = hspec $ do
  describe "Convert a Letter into a printable Char" $ do
    it "Take a Letter and convert it to '_' if Hidden or its character if Guessed." $ do
      letterToChar (Hidden 'c') `shouldBe` '_'
      letterToChar (Guessed 'c') `shouldBe` 'c'

  describe "Determine if a word is completely guessed." $ do
     it "Return True if the word has no Hidden Chars in it, otherwise False." $ do
       complete [(Guessed 'c'), (Guessed 'h'), (Guessed 'a'), (Guessed 'd')] `shouldBe` True
       complete [(Guessed 'c'), (Guessed 'h'), (Hidden 'a'), (Guessed 'd')] `shouldBe` False
       complete [(Hidden 'c'), (Guessed 'h'), (Guessed 'a'), (Guessed 'd')] `shouldBe` False
       complete [(Guessed 'c'), (Guessed 'h'), (Guessed 'a'), (Hidden 'd')] `shouldBe` False
       complete [(Hidden 'c'), (Hidden 'h'), (Hidden 'a'), (Hidden 'd')] `shouldBe` False

  describe "Determine if we've reached the end of the game." $ do
    it "Return True if the word is complete." $ do
      endGame [(Guessed 'c'), (Guessed 'h'), (Guessed 'a'), (Guessed 'd')] (1,10) `shouldBe` True
    it "Return True if all guesses are depleted." $ do
      endGame [(Hidden 'c'), (Hidden 'h'), (Hidden 'a'), (Hidden 'd')] (10,10) `shouldBe` True
    it "Return False if word is not complete or guesses are remaining" $ do
      endGame [(Guessed 'c'), (Hidden 'h'), (Guessed 'a'), (Guessed 'd')] (2,10) `shouldBe` False

