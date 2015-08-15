module Main where

import System.IO
import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class

-- | Represents if a character is discovered.
data Letter = Hidden Char | Guessed Char
-- | Represents a word made up of letters.
type Word = [Letter]
-- | The state of the Hangman game. 
data HangmanState = HangmanState Word (Int,Int) [Char]

main :: IO ()
main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hangman word guesses
  where
    word = ["george","pete","willy"] !! 0
    guesses = 10

-- | The start of the game.
hangman :: String -> Int -> IO ()
hangman word guesses = do
  let word' = fmap (\x -> Hidden x) word
  (hs, s) <- evalStateT looper (HangmanState word' (0, guesses) [])
  showState hs
  case s of
   True -> putStrLn "You've won!"
   False -> putStrLn "You've lost!"

-- | An iteration of the game.
looper ::  StateT HangmanState IO (HangmanState, Bool)
looper = do
  hs@(HangmanState word (guess,guesses) guessed) <- get
  liftIO $ showState hs
  userChar <- liftIO getChar
  let word' = fmap (checkGuess userChar) word
  let hs' = (HangmanState word' (guess+1,guesses) (userChar:guessed))
  case complete word' of
   True -> return (hs', True)
   False -> case guess == guesses of
     True -> return (hs', False)
     False -> do
       put hs'
       looper

-- | Print the state of the game.
showState :: HangmanState -> IO()
showState state@(HangmanState word (guess, guesses) guessed) = do
  putStrLn $
    wordToString word ++
    " " ++
    (show guess) ++ "/" ++ (show guesses)
  putStrLn $
    "Guessed:" ++ (show guessed)

wordToString :: Word -> String
wordToString = (fmap letterToChar)

letterToChar :: Letter -> Char
letterToChar l = case l of
  Hidden x -> '_'
  Guessed x -> x

-- | Transform a Hidden character into a Guessed character.
checkGuess :: Char -> Letter -> Letter
checkGuess c (Hidden x)
  | x == c = Guessed x
checkGuess c x = x

-- | Determine if we've reached the end of the game.
endGame :: Word -> (Int,Int) -> Bool
endGame word (guess,guesses)
  | complete word = True
  | guess == guesses = True
  | otherwise = False


-- | Determine if the word is completely guessed.
complete :: Word -> Bool
complete = all isGuessed

isGuessed :: Letter -> Bool
isGuessed l = case l of
                 Hidden x -> False
                 Guessed x -> True
