module Main where

import Hangman
import qualified Words (list)

import System.IO
import System.Random

main :: IO ()
main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  guesses <- getStdRandom (randomR (5,15))
  wordIndex <- getStdRandom (randomR (0, (length Words.list) - 1))
  let word = Words.list !! wordIndex
  hangman word guesses
