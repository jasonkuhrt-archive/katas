
-- # Numbers In Words
-- http://www.codingdojo.org/cgi-bin/index.pl?KataNumbersInWords

-- Steps Completed --
-- 1 Yes (some caveats, see below)
-- 2 Pending
-- 3 Pending

-- Examples --
-- $ ghci Main.hs
-- $ > main
-- zero
-- one
-- two
-- three
-- four
-- five
-- six
-- seven
-- eight
-- nine
-- ten
-- eleven
-- twelve
-- thirteen
-- fourteen
-- fifteen
-- sixteen
-- seventeen
-- eighteen
-- nineteen
-- twenty
-- twenty one
-- thirty
-- thirty one
-- fourty
-- fourty one
-- fifty
-- fifty one
-- sixty
-- sixty one
-- seventy
-- seventy one
-- eighty
-- eighty one
-- ninety
-- ninety one
-- one hundred
-- one hundred one
-- one hundred ten
-- one hundred eleven
-- one thousand
-- one thousand one
-- one thousand ten
-- one thousand one hundred
-- one thousand one hundred eleven
-- five thousand three hundred eighty
-- nine thousand eight hundred thirty four
-- seven thousand one hundred eighty three
-- eight thousand four hundred twenty one
-- four thousand two hundred thirty four
-- eighty seven thousand three hundred twelve
-- one hundred fourty three thousand five hundred eleven
-- one hundred one
-- one million two hundred thirty four thousand eight hundred seventy six
-- one million fourteen
-- ninety one million two hundred thirty four thousand eight hundred seventy six
-- fourty one million fourteen



{-
TODO The current solution could be more general.

Notice the repeating pattern:

  Ones -> Tens -> Hundreds

in the introduction of e.g.:

  Thousands, Millions, Billions, Trillions, Quadrillion, Quintillion

meaning that the introduction of any of the above affords the representation
of an additional ****THREE**** digit places! e.g.

  Ones     ->                      five quintillion ... eighteen
  Tens     ->               twenty five quintillion ... eighteen
  Hundreds -> eight hundred twenty five quintillion ... eighteen
                                        ^Unit Group

As we can see in the above example the one-time addition of "quintillion"
indeed permits three-time expansion of digits. We will refer to this as a
"unit-group".

So... it seems clear that my current system is flawed in part because it
fails to model so-called unit-groups! :)
-}



type Digit = Int

data DigitPlace =
    Ones
  | Tens
  | Hundreds
  | Thousands
  | TenThousands
  | HundredThousands
  | Millions
  | TenMillions
  -- ... See TODO
  deriving (Show, Enum)



-- Demo --

main :: IO ()
main = putStr . unlines . map numberToWords $ testNumbers
  where
  testNumbers = [0,1,2,3,4,5,6,7,8,9,  10,11,12,13,14,15,16,17,18,19,  20,21,30,31,40,41,50,51,60,61,70,71,80,81,90,91,  100,101,110,111,   1000,1001,1010,1100,1111,5380,9834,7183,8421,  14234,87312,  143511,100001,  1234876,1000014,  91234876,41000014]



-- Number to Words Converter --

numberToWords :: Int -> String
numberToWords = digitsToWords . intToDigits

digitsToWords :: [Digit] -> String
-- In written English the digit "zero" is only ever stated in the case of
-- the number zero (AKA "0") which makes this a core edge-case.
-- The core folding algorithm handles the digit "0" differently as its
-- part of larger number e.g. "10".
digitsToWords [0]    = "zero"
digitsToWords digits =
  unwords . filter (/= "") . resolve (maxDigitPlace digits) $ digits
  where

  -- `Resolve` recursively converts every digit into its word beginning
  -- from the maximum digit place (e.g. Thousands) down to the Ones.
  --
  -- Some incremental conversion results *can* be empty because not all
  -- digits are pronounced in english. For example we do not say
  -- "zero" for the "0" in "101".
  --
  -- We need manual recursion because of the teen edge-case. It requires
  -- access to _two_ digits at once. The leading "1" digit to detect a teen,
  -- and the second digit to detect _which_ teen.
  --
  resolve :: DigitPlace -> [Digit] -> [String]
  resolve _ []     = [""]
  resolve _ [1,n]  = [teenToWord n]
  resolve p (n:ns) = digitToWord p n : resolve (pred p) ns

  maxDigitPlace :: [Digit] -> DigitPlace
  maxDigitPlace = toEnum . subtract 1 . length




-- Digit to Word Converters --

-- Converter dispatcher. This allows us to programatically pick a converter
-- based on the DigitPlace.

digitToWord :: DigitPlace -> Int -> String
digitToWord Ones = onesToWord
digitToWord Tens = tensToWord
digitToWord Hundreds = hundredsToWord
digitToWord Thousands = thousandsToWord
digitToWord TenThousands = tensToWord
digitToWord HundredThousands = hundredsToWord
digitToWord Millions = millionsToWord
digitToWord TenMillions = tensToWord
-- ... See TODO

onesToWord :: Digit -> String
onesToWord 1 = "one"
onesToWord 2 = "two"
onesToWord 3 = "three"
onesToWord 4 = "four"
onesToWord 5 = "five"
onesToWord 6 = "six"
onesToWord 7 = "seven"
onesToWord 8 = "eight"
onesToWord 9 = "nine"
onesToWord _ = ""

teenToWord :: Digit -> String
teenToWord 0 = "ten"
teenToWord 1 = "eleven"
teenToWord 2 = "twelve"
teenToWord 3 = "thirteen"
teenToWord 4 = "fourteen"
teenToWord 5 = "fifteen"
teenToWord 6 = "sixteen"
teenToWord 7 = "seventeen"
teenToWord 8 = "eighteen"
teenToWord 9 = "nineteen"
teenToWord _ = ""

tensToWord :: Digit -> String
tensToWord 2 = "twenty"
tensToWord 3 = "thirty"
tensToWord 4 = "fourty"
tensToWord 5 = "fifty"
tensToWord 6 = "sixty"
tensToWord 7 = "seventy"
tensToWord 8 = "eighty"
tensToWord 9 = "ninety"
tensToWord _ = ""

hundredsToWord :: Digit -> String
hundredsToWord 0 = ""
hundredsToWord digit = (++ " hundred") . onesToWord $ digit

thousandsToWord :: Digit -> String
thousandsToWord 0 = ""
thousandsToWord digit = (++ " thousand") . onesToWord $ digit

millionsToWord :: Digit -> String
millionsToWord 0 = ""
millionsToWord digit = (++ " million") . onesToWord $ digit




-- Helpers --

intToDigits :: Int -> [Digit]
intToDigits = map (read . charToString) . show

charToString :: Char -> String
charToString c = [c]
