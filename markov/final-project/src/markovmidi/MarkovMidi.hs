module MarkovMidi where

import MidiValues as MV
import Data.List
import System.IO  
import System.Directory
import Control.Monad
import Sound.PortMidi
import Data.Map (Map, fromList, member, toList, fromListWith)
import qualified Data.Map as Map
import System.Random
import qualified Sound.MIDI.File.Load as SMFL
import qualified Sound.MIDI.File as SML
import Data.Typeable
import Data.Foldable
import Text.Read 
import qualified Euterpea as E

--------------MIDI--------------------

substring :: String -> String -> Bool
substring (_:_) [] = False
substring xs ys
    | prefix xs ys = True
    | substring xs (tail ys) = True
    | otherwise = False

prefix :: String -> String -> Bool
prefix [] _ = True
prefix (_:_) [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot pred = filter $ not . pred

filterMIDImessages :: [String] -> [String]
filterMIDImessages xs = filterNot (`elem` unwanted) xs
    where 
        unwanted = ["(Control","(Controller","{fromController","7})","empty", "{fromPitch","(TextEvent","B\")","0","120", "12", "3", "/.","MetaEvent","./","120","/.","MIDIEvent","(Cons","{messageChannel","=","Channel","{fromChannel","=","0},","messageBody","=","Voice","(Velocity","{fromVelocity","=","64}))})","./","60","/.","MIDIEvent","(NoteOff","(Pitch"]

getNotesOn :: [String] -> [String]
getNotesOn (x1:x2:xs) = 
    if(x1 == "(NoteOn")
        then x2: getNotesOn xs
        else getNotesOn (x2:xs)
getNotesOn _ = []

getNoteNumbers :: [String] -> [Int]
getNoteNumbers (x:xs) = 
    if substring ("})") x 
        then read (take ((length x)-2) x) : getNoteNumbers xs
        else getNoteNumbers xs
getNoteNumbers _ = [] 

makeLine :: [Int] -> (Int, StdGen) -> [E.Music E.Pitch]
makeLine (x:xs) noteLenGen = (noteLength !! x) : makeLine xs noteLenGen'
    where 
        noteLength  = MV.randomNoteLength !! (fst noteLenGen)
        noteLenGen' = randomR (0, (length MV.randomNoteLength)-1) (snd noteLenGen)
makeLine _ _ = []

getTrackStrs :: Show a => [a] -> [String]
getTrackStrs (x:xs) = show x : getTrackStrs xs
getTrackStrs _ = []

noteNumbers :: [[String]] -> [[Int]]
noteNumbers msgs = map getNoteNumbers $ map getNotesOn msgs

buildPiece :: [E.Music a] -> E.Music a
buildPiece (x:xs) = x E.:=: buildPiece xs
buildPiece _ = E.rest E.wn

bigFilter :: [String] -> [[String]]
bigFilter ts = map (filterNot (substring "}))})")) $ map filterMIDImessages $ map words ts

getFirstLeftHand :: (Eq a, Enum a, Num a) => a -> Int -> a
getFirstLeftHand rh idx
    |elem rh [40..46] = (rh-12)+r
    |elem rh [47..42] = (rh-18)+r
    |elem rh [53..58] = (rh-24)+r
    |elem rh [59..64] = (rh-30)+r
    |elem rh [65..70] = (rh-36)+r
    where
        r = [0, -3, 4, -7, 7] !! idx --major?
    --  r = [0, -3, 3, -7, 7] !! idx --minor?
getFirstLeftHand rh _ = rh-24

adjustLeftHand :: (Ord a, Num a) => [a] -> [a]
adjustLeftHand (x:xs) = 
    if x > 50 
        then x-12: adjustLeftHand xs
        else x:    adjustLeftHand xs
adjustLeftHand _ = []

adjustRightHand :: (Ord a, Num a) => [a] -> [a]
adjustRightHand (x:xs) = 
    if x < 50 
        then x+12: adjustRightHand xs
        else x:    adjustRightHand xs
adjustRightHand _ = []

-------------------MARKOV-------------------

makePairs :: [Int] -> [(Int, Int)]
makePairs [x] = []
makePairs (x:xs) = (x, head xs) : makePairs xs

makeMarkovChain :: (Ord a) => [(a, b)] -> [(a, [b])]
makeMarkovChain xs = (Map.toList . Map.fromListWith (++) . map (\(x,y) -> (x,[y]))) xs

generateMarkovPiece :: [(Int, [Int])] -> Int -> Int -> (Int, StdGen) -> [Int] 
generateMarkovPiece chain note pieceLen nextGen =
    if pieceLen > 0 
        then nextNote : (generateMarkovPiece chain nextBigIndex (pieceLen-1) nextGen') 
        else [nextNote]
        where 
            nextNote = ((snd (chain !! note)) !! fst nextGen) 
            nextGen' = randomR (0, (length (snd (chain !! nextBigIndex))-1)) (snd nextGen) -- minus 1???
            nextBigIndex =
                case findIndex ((nextNote ==) . fst) chain of
                    Just i -> i
                    Nothing -> 0

randomIndex  :: (Num a, Random a) => a -> a -> IO (a, StdGen)
randomIndex  a b = do
    g <- newStdGen
    return (randomR (a,b-1) g)

main = do  

    putStrLn "Midi Module: Enter Composer (bach, beeth, chopin, mozart, tschai): "
    composer' <- getLine
    let composer = filterNot (==' ') composer'
    dirContents <- listDirectory $ "composers/"++composer
    let files = map ((("composers/"++)composer++"/")++) dirContents
    --TEST WITH SINGLE FILE
    -- let test_file = "bach/all_bach/catech7.mid"  
    -- file <- SMFL.fromFile test_file
    -- SMFL.showFile test_file
    -- putStrLn ("type of file is: " ++ (show (typeOf file)))
    
    -- let tracks = SML.getTracks file
    -- let trackStrs = getTrackStrs $ tail tracks 
    -- print $ map (take 10) $ map words trackStrs
    -- let noteMessages = bigFilter trackStrs
    -- let tracksAndNotes = zip [1..] $ noteNumbers noteMessages
    
    -- let hands = map E.line $ map makeLine (map snd tracksAndNotes) 
    -- print $ show hands

    -- let piece = buildPiece hands
    -- E.play piece


    --ALL FILES 
    files'              <- mapM SMFL.fromFile files
    let tracks          = map SML.getTracks files'
    let trackStrs       = map getTrackStrs $ map tail tracks 

    let noteMessages    = map bigFilter trackStrs
    let tracksAndNotes  = map noteNumbers noteMessages
    let allNoteVals     = concat $ concat tracksAndNotes
    let allNoteVals'    = [x | x<-allNoteVals, x >= 21, x <= 108] --88 keys

    let notePairs       = makePairs allNoteVals'
    let noteChain       = makeMarkovChain notePairs

    putStrLn "Enter the length of the piece: "

    l <- getLine
    let pieceLen = read l :: Int 
    let chainLen                = length noteChain    --77

    --RIGHT HAND
    firstNoteGen                <- randomIndex 40 70  --60 is middle C 90 is high F#
    let fstRHNoteIndex          = fst firstNoteGen
    let firstRHNoteChain        = noteChain !! fstRHNoteIndex
    let firstRHNote             = fst firstRHNoteChain
    let nextGen                 = snd firstNoteGen
    -- putStrLn "first RH: "
    -- print fstRHNoteIndex
    smallRHIndexGen             <- randomIndex 0 $ length (snd firstRHNoteChain)
    let rightHand               =  firstRHNote : generateMarkovPiece noteChain fstRHNoteIndex pieceLen smallRHIndexGen
    let rightHand'              = adjustRightHand rightHand

    --LEFT HAND
    leftStochasticIdxGen        <- randomIndex 0 6 
    let idx                     = fst leftStochasticIdxGen :: Int
    -- putStrLn "stochastic lh interval:"
    -- print idx
    -- --check idx within range of 77 notes!
    let fstLHNoteIndex          = getFirstLeftHand fstRHNoteIndex idx
    let firstLHNoteChain        = noteChain !! fstLHNoteIndex
    let firstLHNote             = fst firstLHNoteChain
    -- putStrLn "first LH:"
    -- print fstLHNoteIndex
    smallLHIndexGen             <- randomIndex 0 $ length (snd firstLHNoteChain)
    let leftHand                = firstLHNote : generateMarkovPiece noteChain fstLHNoteIndex  pieceLen smallLHIndexGen
    let leftHand'               = adjustLeftHand leftHand

    rightNoteLenGen             <- randomIndex 0 (length MV.randomNoteLength) 
    leftNoteLenGen              <- randomIndex 0 (length MV.randomNoteLength) 

    let rightHandPart = E.line $ makeLine rightHand' rightNoteLenGen
    let leftHandPart  = E.line $ makeLine  leftHand' leftNoteLenGen
    
    -- RIGHT PART
    putStrLn "Right Hand"
    print rightHand'
    let piece = buildPiece [rightHandPart]
    E.play piece

    -- LEFT PART
    putStrLn "Left Hand:"
    print leftHand'
    let piece' = buildPiece [leftHandPart]
    E.play piece'

    -- TWO HANDS
    putStrLn "Both Hands (brace yourselves...)"
    let piece'' = buildPiece [rightHandPart, leftHandPart] 
    E.play piece''
    print "end"



