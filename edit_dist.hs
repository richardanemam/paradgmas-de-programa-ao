import qualified Data.Map.Strict as Map
import           Data.Char
import           Data.Maybe
import           Data.List
import           System.IO

lowerWords :: String -> String
lowerWords word = map toLower word

-- | Remove punctuation from text String.
removePunc :: String -> String
removePunc word = [w | w <- word, not (w `elem` ",.?!-:;\"\'")]

wordsList :: String -> [String]
wordsList word = words word

-- | maps how many times a word appears in the file
countWords :: [String] -> [(String,Int)]
countWords xs = map (\w -> (head w, length w)) $ group $ sort xs

-- | Probability of word.
probability :: String -> [(String, Int)] -> Double
probability word dictOfWords = (/ n) $ fromIntegral $ fromMaybe 0 (Map.lookup word (Map.fromList dictOfWords) :: Maybe Int)
  where
    n = fromIntegral $ Map.foldl' (+) 0 (Map.fromList dictOfWords)

helper :: [String] -> [(String, Int)] -> String
helper (x:xs) dictOfWords | xs == []  = x
                          | otherwise = if p2 > p
                                        then helper xs dictOfWords
                                        else helper (x:(drop 1 xs)) dictOfWords
                                        where p2 = probability (xs !! 0) dictOfWords
                                              p  = probability x dictOfWords

correction :: String -> [(String, Int)] -> String
correction word dictOfWords = helper list dictOfWords
  where list = candidates word dictOfWords

candidates :: String -> [(String, Int)] -> [String]
candidates word dictOfWords = head $ filter (not . null) s
  where
    s = [ known [word] dictOfWords
        , known (edits1 word) dictOfWords
        , known (edits2 word) dictOfWords
        , [word]
        ]

known :: [String] -> [(String, Int)] -> [String]
known words' dictOfWords = [ w | w <- words', Map.member w (Map.fromList dictOfWords)]

-- | All edits that one two edits away from word.
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

main :: IO ()
main = do

  word <- readFile "Todas-as-obras-Machado-de-Assis.txt"
  let dictOfWords = countWords(wordsList(lowerWords(removePunc (word))))

  wordToCorrect <- getLine            --wordToCorrect :: String
  let rightWordBe = correction wordToCorrect dictOfWords
  
  print (rightWordBe)
