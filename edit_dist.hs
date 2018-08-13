import qualified Data.Map.Strict as Map
import           Data.Char
import           Data.Maybe
import           Data.List
import           Data.Ord
import           System.IO
import           System.IO.Unsafe
--import Data.List.Extras.Argmax as AM

{-# NOINLINE file' #-}
file' :: String
file' = do
   word' <- (unsafePerformIO(readFile "Todas-as-obras-Machado-de-Assis.txt"))
   return word'

dictOfWords :: [(String, Int)]
dictOfWords = (countWords(wordsList(lowerWords(removePunc (file')))))

--(countWords(wordsList(lowerWords(removePunc word'))))
lowerWords :: String -> String
lowerWords word = map toLower word

-- Remove punctuation from text String.
removePunc :: String -> String
removePunc word = [w | w <- word, not (w `elem` ",.?!-:;\"\'")]

wordsList :: String -> [String]
wordsList word  = words word

countWords :: [String] -> [(String,Int)]
countWords xs = map (\w -> (head w, length w)) $ group $ sort xs

-- | Probability of @word@.
probability :: String -> Double
probability word = (/ n) $ fromIntegral $ fromMaybe 0 (Map.lookup word (Map.fromList dictOfWords) :: Maybe Int)
  where
    n = fromIntegral $ Map.foldl' (+) 0 (Map.fromList dictOfWords)

helper :: [String] -> String
helper (x:xs) | xs == []  = x
              | otherwise = if p2 > p
                       then helper xs
                       else helper (x:(drop 1 xs))
                       where p2 = probability (xs !! 0)
                             p = probability x

correction :: String -> String
correction word = helper list
  where list = candidates word

candidates :: String -> [String]
candidates word = head $ filter (not . null) s
  where
    s = [ known [word]
        , known $ edits1 word
        , known $ edits2 word
        , [word]
        ]

known :: [String] -> [String]
known words' = [ w | w <- words', Map.member w (Map.fromList dictOfWords)]

edits1 :: String -> [String]
edits1 word = deletes ++ transposes ++ replaces ++ inserts
  where
    letters    = "abcdefghijklmnopqrstuvwxyz"
    splits     = [ splitAt i word                  | i <- [1 .. length word] ]
    deletes    = [ l ++ tail r                     | (l,r) <- splits, (not . null) r ]
    transposes = [ l ++ r !! 1 : head r : drop 2 r | (l,r) <- splits, length r > 1 ]
    replaces   = [ l ++ c : tail r                 | (l,r) <- splits, (not . null) r, c <- letters ]
    inserts    = [ l ++ c : r                      | (l,r) <- splits, c <- letters]

-- | All edits that are two edits away from @word@.
edits2 :: String -> [String]
edits2 word = [ e2 | e1 <- edits1 word, e2 <- edits1 e1 ]

--main :: IO ()
main = do

  --print $ dictOfWords

  wordToCorrect <- getLine            --wordToCorrect :: String
  let rightWordBe = correction wordToCorrect
  print(rightWordBe)
