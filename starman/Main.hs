{- README
-- Code Kata - Starman

-- Specification

In this single-doPlayTurner, text-based game, there is a word which the player needs to guess. For each turn of the game, the player guesses a single letter. If that letter is correct, then the guessed letters are player in the correct places in the word. If that letter is incorrect, then the user loses a star. Once the user has no stars left, they have lost the game. However if the user guesses all the letters in the word, they have won the game.
-}
module Main where

import Data.Char (isLetter)
import Data.List (intersect, intersperse)



main :: IO ()
main = doPlayTurn . make 3 $ "hello"



type Game    = (Werd, Guesses, Stars)
type Guesses = [Letter]
type Stars   = Int
type Werd    = String
type Letter  = Char



-- IO --

doPlayTurn :: Game -> IO ()
doPlayTurn game = do
  putStrLn . showStatus $ game
  if isWin game
    then putStrLn "You won!"
  else if isLose game
    then putStrLn "You lost!"
  else
    doGuessLetter game

doGuessLetter :: Game -> IO ()
doGuessLetter game = do
  putStr "Guess a letter: "
  maybeValueLetter <- getLine
  if null maybeValueLetter
    then tryAgain "Cannot guess nothing."
  else if length maybeValueLetter > 1
    then tryAgain "Cannot guess multiple letters at once."
  else if not . all isLetter $ maybeValueLetter
    then tryAgain "Can only guess letters (a-z)."
  else if isGuessed (head maybeValueLetter) game
    then tryAgain $ "You have already guessed "++show maybeValueLetter++"."
  else
    doPlayTurn . guessLetter (head maybeValueLetter) $ game
  where
  tryAgain reason = do
    putStrLn (reason ++ " Try again.")
    doGuessLetter game





-- Graphics --

showStatus :: Game -> String
showStatus game =
  showStars game
  ++ "  |  " ++ showWerd game
  ++ "  |  Guesses: " ++ showGuesses game

showStars :: Game -> String
showStars (_,_, stars) =
  (++ " ★") . show $ stars

showGuesses :: Game -> String
showGuesses (_, "", _)      =  "(nothing)"
showGuesses (_, guesses, _) = intersperse ' ' (reverse guesses)

showWerd :: Game -> String
showWerd (werd, guesses, _) =
  zipWith showLetter werd (fmap (flip elem guesses) werd)
  where
  showLetter :: Letter -> Bool -> Char
  showLetter _      False = '_'
  showLetter letter _     = letter



-- Logic --

isWin :: Game -> Bool
isWin (werd, guesses, _) =
  length werd == length (intersect werd guesses)

isLose :: Game -> Bool
isLose (_,_, stars) = stars <= 0

isGuessed :: Letter -> Game -> Bool
isGuessed letter (_, guesses, _) = elem letter guesses

guessLetter :: Letter -> Game -> Game
guessLetter letter (werd, guesses, stars)
  | elem letter werd = (werd, letter : guesses, stars    )
  | otherwise        = (werd, letter : guesses, stars - 1)

make :: Stars -> Werd -> Game
make stars werd = (werd, [], stars)
