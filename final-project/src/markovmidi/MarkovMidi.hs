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

--------------HAND ADJUSTMENTS--------------

getFirstLeftHand :: (Eq a, Enum a, Num a) => a -> Int -> a
getFirstLeftHand rh idx
    |elem rh [40..46] = (rh-12)+r
    |elem rh [47..52] = (rh-18)+r
    |elem rh [53..58] = (rh-24)+r
    |elem rh [59..64] = (rh-30)+r
    |elem rh [65..70] = (rh-36)+r
    where
        r = [0, -3, 4, -7, 7] !! idx
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

keepInRange :: [Int] -> Int -> Int -> [Int]
keepInRange (x:xs) chainLen tonic = 
    if x > chainLen || x < 0
        then tonic : keepInRange xs chainLen tonic
        else x : keepInRange xs chainLen tonic
keepInRange _ _ _ = []

---------------------KEY-------------------------

setMood :: (Eq a, Num a) => a -> [Int] -> Int -> [Int]
setMood key = 
    if key == 1
        then setMajorKey
    else setMinorKey

setMajorKey :: [Int] -> Int -> [Int]
setMajorKey (x:xs) tonic =
    if elem x notesInKey
        then x : setMajorKey xs tonic
        else (x+nearestNoteInKey) : setMajorKey xs tonic
    where 
        notesInKey = map (tonic +) intervals ++ map (tonic -) intervals'
        intervals = [0, 2, 4, 5, 7, 9, 11, 12, 14, 16, 17, 19, 21, 23, 24, 26, 28, 29, 31, 33, 35, 36, 38, 40, 41, 43, 45, 47]
        intervals'= [0, 1, 3, 5, 7, 8, 10, 12, 13, 15, 17, 19, 20, 22, 24, 25, 27, 29, 31, 32, 34, 36, 37, 39, 41, 43, 44, 46] 
        nearestNoteInKey = minimum $ map abs $ map (\y -> x - y) notesInKey
        abs = (\z -> if z >= 0 then z else z*(-1)) 
setMajorKey _ _ = []

setMinorKey :: [Int] -> Int -> [Int]
setMinorKey (x:xs) tonic =
    if elem x notesInKey
        then x : setMinorKey xs tonic
        else (x+nearestNoteInKey) : setMinorKey xs tonic
    where 
        notesInKey = map (tonic +) intervals ++ map (tonic -) intervals'
        intervals = [0, 2, 3, 5, 7, 8, 9, 10, 12, 14, 15, 17, 19, 20, 21, 22, 24, 26, 27, 29, 31, 32, 33, 34, 36, 38, 39, 41, 43, 45, 46]
        intervals'= [0, 1, 2, 4, 5, 7, 9, 10, 12, 13, 14, 16, 17, 19, 21, 22, 24, 25, 26, 28, 29, 31, 33, 34, 36, 37, 38, 40, 41, 43, 45]
        nearestNoteInKey = minimum $ map abs $ map (\y -> x - y) notesInKey
        abs = (\z -> if z >= 0 then z else z*(-1)) 
setMinorKey _ _ = []

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

    putStrLn "Midi Module: Enter Composer (bach, beeth, chopin, mozart, tschai) or just type 'all': "
    composer' <- getLine
    let composer = filterNot (==' ') composer'
    dirContents <- listDirectory $ "composers/"++composer
    let files = map ((("composers/"++)composer++"/")++) dirContents

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

    putStrLn "Enter 1 for Major, 2 for Minor: "
    k <- getLine
    let key      = read k :: Int

    --RIGHT HAND
    
    firstNoteGen                <- randomIndex 40 64  --60 is middle C 90 is high F#
    let fstRHNoteIndex          = fst firstNoteGen
    let firstRHNoteChain        = noteChain !! fstRHNoteIndex
    let firstRHNote             = fst firstRHNoteChain
    let nextGen                 = snd firstNoteGen
    
    -- putStrLn "first RH: "
    -- print fstRHNoteIndex
    
    smallRHIndexGen             <- randomIndex 0 $ length (snd firstRHNoteChain)
    let rightHand               = firstRHNote : generateMarkovPiece noteChain fstRHNoteIndex pieceLen smallRHIndexGen
    let rightHand'              = adjustRightHand $ keepInRange ((setMood key) rightHand firstRHNote) chainLen firstRHNote

    --LEFT HAND
    
    leftStochasticIdxGen        <- randomIndex 0 4 
    let idx                     = fst leftStochasticIdxGen :: Int
    
    -- putStrLn "stochastic lh interval:"
    -- print idx
    
    let fstLHNoteIndex          = getFirstLeftHand fstRHNoteIndex idx
    let firstLHNoteChain        = noteChain !! fstLHNoteIndex
    let firstLHNote             = fst firstLHNoteChain

    -- putStrLn "first LH:"
    -- print fstLHNoteIndex
    
    smallLHIndexGen             <- randomIndex 0 $ length (snd firstLHNoteChain)
    let leftHand                = firstLHNote : generateMarkovPiece noteChain fstLHNoteIndex  pieceLen smallLHIndexGen
    let leftHand'               = adjustLeftHand $ keepInRange ((setMood key) leftHand firstRHNote) chainLen firstRHNote --RH note is tonic

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



