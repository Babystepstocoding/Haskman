import System.IO
import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class

type Token = (Char,Bool)
data HangmanState = HangmanState [Token] (Int,Int) [Char]

main :: IO ()
main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hangman "hello" 10

hangman :: String -> Int -> IO ()
hangman word guesses = do
  let word' = fmap (\x -> (x,False)) word
  s <- evalStateT looper (HangmanState word' (0, guesses) [])
  case s of
   True -> putStrLn "You've won!"
   False -> putStrLn "You've lost!"

looper ::  StateT HangmanState IO Bool
looper = do
  hs@(HangmanState word (guess,guesses) guessed) <- get
  liftIO $ showState hs
  userChar <- liftIO getChar
  let word' = fmap (checkGuess userChar) word
  case complete word' of
   True -> return True
   False -> case guess == guesses of
     True -> return False
     False -> do
       put (HangmanState word' (guess+1,guesses) (userChar:guessed))
       looper

showState :: HangmanState -> IO()
showState state@(HangmanState word (guess, guesses) guessed) = do
  putStrLn $
    fmap toHoleyString word ++
    " " ++
    (show guess) ++ "/" ++ (show guesses)
  putStrLn $
    "Guessed:" ++ (show guessed)

toHoleyString :: Token -> Char
toHoleyString (c,True) = c
toHoleyString (_,False) = '_'

checkGuess :: Char -> Token -> Token
checkGuess c (x,y)
  | c == x = (x,True)
  | otherwise = (x,y)

endGame :: [Token] -> (Int,Int) -> Bool
endGame word (guess,guesses)
  | complete word = True
  | guess == guesses = True
  | otherwise = False

complete :: [Token] -> Bool
complete [] = True
complete (x:xs)
  | state /= False = complete xs
  | otherwise = False
  where
    state = snd x
