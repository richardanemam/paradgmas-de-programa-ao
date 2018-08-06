module Spelling (TrainingDict, nWords, correct) where

import qualified Data.Map.Strict as M
import qualified Data.ByteString.Char8 as B
import qualified Data.Set as S
import           Data.Ord (comparing)
import           Data.List (sortBy, foldl')
import           Data.Char (toLower, isAlpha)
import Data.Char
import System.IO

type WordSet = S.Set String
type TrainingDict = M.Map String Int

--Todas-as-obras-Machado-de-Assis.txt

nWords :: IO TrainingDict
nWords = do
  ws <-  B.readFile "teste.txt"
  return (train(lowerWords(removePunc(B.unpack $ ws))))

lowerWords :: String -> [String]
lowerWords ws = words (map toLower ws)

-- Remove punctuation from text String.
removePunc :: String -> String
removePunc xs = [x | x <- xs, not (x `elem` ",.?!-:;\"\'")]

train :: [String] -> TrainingDict
train = foldl' (\acc x -> M.insertWith (+) x 1 acc) M.empty

edits1 :: String -> WordSet
edits1 word = S.fromList $ deletes ++ transposes ++ replaces ++ inserts
  where
    alphabet    = ['a'..'z']
    splits     = [ splitAt i word                  | i <- [1 .. length word] ]
    deletes    = [ l ++ tail r                     | (l,r) <- splits, (not . null) r ]
    transposes = [ l ++ r !! 1 : head r : drop 2 r | (l,r) <- splits, length r > 1 ]
    replaces   = [ l ++ c : tail r                 | (l,r) <- splits, (not . null) r, c <- alphabet ]
    inserts    = [ l ++ c : r                      | (l,r) <- splits, c <- alphabet]

edits2 :: String -> WordSet
edits2 = S.foldl' S.union S.empty . S.map edits1 . edits1

knownEdits2 :: String -> TrainingDict -> WordSet
knownEdits2 w nwords = edits2 w `S.intersection` M.keysSet nwords

known :: WordSet -> TrainingDict -> WordSet
known inputSet nwords = inputSet `S.intersection` M.keysSet nwords

choices :: String -> TrainingDict -> WordSet
choices w ws = foldr orNextIfEmpty (S.singleton w)
   [   known (S.singleton w) ws,
       known (edits1 w) ws,
       knownEdits2 w ws
   ]
   where orNextIfEmpty x y = if S.null x then y else x

chooseBest :: WordSet -> TrainingDict -> B.ByteString
chooseBest ch ws = maximumBy (compare `on` (\w -> M.findWithDefault 0 w ws)) (S.toList ch)

correct :: TrainingDict -> String -> String
correct ws w = chooseBest (choices w ws) ws
