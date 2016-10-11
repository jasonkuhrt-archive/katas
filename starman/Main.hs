{- README
-- Code Kata - Starman

-- Specification

In this single-doPlayTurner, text-based game, there is a word which the player needs to guess. For each turn of the game, the player guesses a single letter. If that letter is correct, then the guessed letters are player in the correct places in the word. If that letter is incorrect, then the user loses a star. Once the user has no stars left, they have lost the game. However if the user guesses all the letters in the word, they have won the game.
-}
module Main where

import Data.Char (isLetter)



main :: IO ()
main = doPlayTurn . make 3 $ "hello"



type Game   = (Werd, Mask, Stars)
type Stars  = Int
type Mask   = [Bool]
type Werd   = String
type Letter = Char



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
  else
    doPlayTurn . guessLetter (head maybeValueLetter) $ game
  where
  tryAgain reason = do
    putStrLn (reason ++ " Try again.")
    doGuessLetter game





-- Graphics --

showStatus :: Game -> String
showStatus game@(_,_, stars) =
  show stars ++ " ★    " ++ showWerd game

showWerd :: Game -> String
showWerd (werd, mask, _) =
  zipWith showLetter werd mask

showLetter :: Letter -> Bool -> Char
showLetter _      False = '_'
showLetter letter _     = letter



-- Logic --

isWin :: Game -> Bool
isWin (_, mask, _) = and mask

isLose :: Game -> Bool
isLose (_,_, stars) = stars <= 0

guessLetter :: Letter -> Game -> Game
guessLetter letter (werd, mask, stars)
  | elem letter werd = (werd, mask', stars    )
  | otherwise        = (werd, mask , stars - 1)
  where
  (_, mask') = unzip $ zipWith go werd mask
  go letter' True  = (letter', True)
  go letter' False
    | letter == letter' = (letter', True)
    | otherwise         = (letter', False)

make :: Stars -> Werd -> Game
make stars werd = (werd, mask, stars) where
  mask = replicate (length werd) False
