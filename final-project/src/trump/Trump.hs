module Trump where

import System.IO  
import Control.Monad
import Data.List
import GHC.Stack
import Text.Read 
import qualified Data.Char as Char
import Data.Map (Map, fromList, member, toList, fromListWith)
import qualified Data.Map as Map
import System.Random
import Debug.Trace

lowerize :: String -> String
lowerize (head:tail) = Char.toLower head : map Char.toLower tail
lowerize [] = []

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot pred = filter $ not . pred

makePairs :: [String] -> [(String, String)]
makePairs [x] = []
makePairs (x:xs) = (x, head xs) : makePairs xs

makeMarkovChain :: (Ord a) => [(a, b)] -> [(a, [b])]
makeMarkovChain xs =(Map.toList . Map.fromListWith (++) . map (\(x,y) -> (x,[y]))) xs

randomList :: Int -> StdGen -> [Int]
randomList  n = take n . unfoldr (Just . random)

randomIndex  :: (Num a, Random a) => a -> a -> IO (a, StdGen)
randomIndex  a b = do
    g <- newStdGen
    return (randomR (a,b-1) g)

generateMarkovSpeech :: [(String, [String])] -> Int -> Int -> (Int, StdGen) -> [String] 
generateMarkovSpeech chain word speechLen nextGen =
    if speechLen > 0 
        then nextWord : (generateMarkovSpeech chain nextBigIndex (speechLen-1) nextGen') 
        else [nextWord]
        where 
            nextWord = ((snd (chain !! word)) !! fst nextGen) 
            nextGen' = randomR (0, (length (snd (chain !! nextBigIndex)) - 1)) (snd nextGen)
            nextBigIndex =
                case findIndex ((nextWord ==) . fst) chain of
                    Just i -> i
                    Nothing -> 0

isTrump :: String -> String
isTrump speaker = 
    if speaker == "trump"
        then "I know words. I have the best words, for instance: "
    else "Here is the generated text:"

main = do  

        putStrLn "Simple Markov text maker."
        putStrLn "Enter the voice to train on (trump, woolf, yeats, shakepeare, joyce, etc):"
        speaker'  <- getLine 
        let speaker = filterNot (== ' ') speaker'
        let file = "voices/"++speaker++".txt"
        contents <- readFile file
        putStrLn "Enter the length of the output:"
        x <- getLine
        let speechLen = read x :: Int 

        --Read in text and create Markov chain
        -- contents       <- readFile "speeches.txt"
        -- contents       <- readFile "joyce.txt"
        -- contents       <- readFile "shakespeare.txt"
        -- contents       <- readFile "yeats.txt"
        -- contents       <- readFile "bible.txt"
        -- contents       <- readFile "woolf.txt"
        
        let wordlist   = words contents
        let lowerwords = map lowerize wordlist
        let pairs      = makePairs lowerwords
        let chain      = makeMarkovChain pairs
        let chainLen   = length chain 
    
        --Start the Markov chain 
        firstWordGen <- randomIndex 0 chainLen
        let firstWordIndex = fst (firstWordGen)
        let firstWordChain = chain !! firstWordIndex
        smallIndexGen <- randomIndex 0 (length (snd firstWordChain))
        let firstWord = fst firstWordChain
        let nextGen = snd firstWordGen
        
        -- print chain
        -- print firstWordGen
        -- print firstWordChain
        -- putStrLn "first word"
        -- print firstWord
        -- putStrLn "first word index"
        -- print firstWordIndex
        -- print nextGen  
        -- print $ length $ snd $ firstWordChain

        let speech = generateMarkovSpeech chain firstWordIndex speechLen smallIndexGen
        
        print $ isTrump speaker
        print $ unwords speech
        

