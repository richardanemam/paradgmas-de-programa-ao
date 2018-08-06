import qualified Data.Map.Strict as M
--import Data.Text (Text)
import Data.Char
import System.IO
--import Data.String.ToString


--Todas-as-obras-Machado-de-Assis.txt

readWords :: IO [String]
readWords = do text <- readFile "teste.txt"
               return (words (removePunc (map toLower text)))


-- Remove punctuation from text String.
removePunc :: String -> String
removePunc xs = [x | x <- xs, not (x `elem` ",.?!-:;\"\'")]

--(splitWhen  $ map toLower words)
lowerWords :: String -> String
lowerWords words = map toLower words

--wordsLen :: IO [String] -> Int
wordsLen =  length readWords
--probability :: String -> Double
--probability w = fromIntegral ((count w) `div` (length nWords))

--count :: String -> Int
--count w = length[w | w <- nWords, Map.member w nWords]


--known :: [String] -> [String]
--known dictionary = [ w | w <- dictionary, Map.member w nWords]

edits1 :: String -> [String]
edits1 word = deletes ++ transposes ++ replaces ++ inserts
  where
    alphabet    = ['a'..'z']
    splits     = [ splitAt i word                  | i <- [1 .. length word] ]
    deletes    = [ l ++ tail r                     | (l,r) <- splits, (not . null) r ]
    transposes = [ l ++ r !! 1 : head r : drop 2 r | (l,r) <- splits, length r > 1 ]
    replaces   = [ l ++ c : tail r                 | (l,r) <- splits, (not . null) r, c <- alphabet ]
    inserts    = [ l ++ c : r                      | (l,r) <- splits, c <- alphabet]

edits2 :: String -> [String]
edits2 word = [ e2 | e1 <- edits1 word, e2 <- edits1 e1 ]
