    -- print chain
    -- print firstWordGen
    -- print firstWordChain
    -- putStrLn "first word"
    -- print firstWord
    -- putStrLn "first word index"
    -- print firstWordIndex
    -- print nextGen  
    -- print $ length $ snd $ firstWordChain

    --TEST WITH SINGLE FILE
    let test_file = "composers/bach/catech7.mid"  
    file <- SMFL.fromFile test_file
    SMFL.showFile test_file
    putStrLn ("type of file is: " ++ (show (typeOf file)))
    
    let tracks = SML.getTracks file
    let trackStrs = getTrackStrs $ tail tracks 
    print $ map (take 10) $ map words trackStrs
    let noteMessages = bigFilter trackStrs
    let tracksAndNotes = zip [1..] $ noteNumbers noteMessages
    
    let hands = map E.line $ map makeLine (map snd tracksAndNotes) 
    print $ show hands

    let piece = buildPiece hands
    E.play piece