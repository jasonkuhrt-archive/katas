import Prelude hiding (Maybe(..), head)
import Data.List (intercalate)
import Data.Char (chr)



main :: IO ()
main = do
  print . head . alphas $ 24
  print . head . alphas $ 0

alphas :: Int -> [Char]
alphas 0 = []
alphas n = fmap chr $ take n [97..]



data Maybe a =
    Nothing
  | Just a
  deriving (Show)

head :: [a] -> Maybe a
head []    = Nothing
head (x:_) = Just x
