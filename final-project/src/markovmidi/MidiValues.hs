module MidiValues where

import Euterpea

midi_notes = [12..119] --12 because c 0 starts at 12 and b 8 is 119

wholes   = replicate 3  wns 
halfs    = replicate 5  hns
quarters = replicate 12 qns
eighths  = replicate 6 ens
sixtns   = replicate 4 sns 
thtysnds = replicate 2 tns

randomNoteLength = concat [wholes, halfs, quarters, eighths, sixtns, thtysnds] --figure out some kind of repeat function for probs.

wns :: [Music Pitch]
wns =  [c 0 wn, cs 0 wn, d 0 wn, ds 0 wn, e 0 wn, f 0 wn, fs 0 wn, g 0 wn, gs 0 wn, a 0 wn, as 0 wn, b 0 wn, 
        c 1 wn, cs 1 wn, d 1 wn, ds 1 wn, e 1 wn, f 1 wn, fs 1 wn, g 1 wn, gs 1 wn, a 1 wn, as 1 wn, b 1 wn, 
        c 2 wn, cs 2 wn, d 2 wn, ds 2 wn, e 2 wn, f 2 wn, fs 2 wn, g 2 wn, gs 2 wn, a 2 wn, as 2 wn, b 2 wn, 
        c 3 wn, cs 3 wn, d 3 wn, ds 3 wn, e 3 wn, f 3 wn, fs 3 wn, g 3 wn, gs 3 wn, a 3 wn, as 3 wn, b 3 wn, 
        c 4 wn, cs 4 wn, d 4 wn, ds 4 wn, e 4 wn, f 4 wn, fs 4 wn, g 4 wn, gs 4 wn, a 4 wn, as 4 wn, b 4 wn, 
        c 5 wn, cs 5 wn, d 5 wn, ds 5 wn, e 5 wn, f 5 wn, fs 5 wn, g 5 wn, gs 5 wn, a 5 wn, as 5 wn, b 5 wn, 
        c 6 wn, cs 6 wn, d 6 wn, ds 6 wn, e 6 wn, f 6 wn, fs 6 wn, g 6 wn, gs 6 wn, a 6 wn, as 6 wn, b 6 wn, 
        c 7 wn, cs 7 wn, d 7 wn, ds 7 wn, e 7 wn, f 7 wn, fs 7 wn, g 7 wn, gs 7 wn, a 7 wn, as 7 wn, b 7 wn,  
        c 8 wn, cs 8 wn, d 8 wn, ds 8 wn, e 8 wn, f 8 wn, fs 8 wn, g 8 wn, gs 8 wn, a 8 wn, as 8 wn, b 8 wn]

hns :: [Music Pitch]
hns =  [c 0 hn, cs 0 hn, d 0 hn, ds 0 hn, e 0 hn, f 0 hn, fs 0 hn, g 0 hn, gs 0 hn, a 0 hn, as 0 hn, b 0 hn, 
        c 1 hn, cs 1 hn, d 1 hn, ds 1 hn, e 1 hn, f 1 hn, fs 1 hn, g 1 hn, gs 1 hn, a 1 hn, as 1 hn, b 1 hn, 
        c 2 hn, cs 2 hn, d 2 hn, ds 2 hn, e 2 hn, f 2 hn, fs 2 hn, g 2 hn, gs 2 hn, a 2 hn, as 2 hn, b 2 hn, 
        c 3 hn, cs 3 hn, d 3 hn, ds 3 hn, e 3 hn, f 3 hn, fs 3 hn, g 3 hn, gs 3 hn, a 3 hn, as 3 hn, b 3 hn, 
        c 4 hn, cs 4 hn, d 4 hn, ds 4 hn, e 4 hn, f 4 hn, fs 4 hn, g 4 hn, gs 4 hn, a 4 hn, as 4 hn, b 4 hn, 
        c 5 hn, cs 5 hn, d 5 hn, ds 5 hn, e 5 hn, f 5 hn, fs 5 hn, g 5 hn, gs 5 hn, a 5 hn, as 5 hn, b 5 hn, 
        c 6 hn, cs 6 hn, d 6 hn, ds 6 hn, e 6 hn, f 6 hn, fs 6 hn, g 6 hn, gs 6 hn, a 6 hn, as 6 hn, b 6 hn, 
        c 7 hn, cs 7 hn, d 7 hn, ds 7 hn, e 7 hn, f 7 hn, fs 7 hn, g 7 hn, gs 7 hn, a 7 hn, as 7 hn, b 7 hn,  
        c 8 hn, cs 8 hn, d 8 hn, ds 8 hn, e 8 hn, f 8 hn, fs 8 hn, g 8 hn, gs 8 hn, a 8 hn, as 8 hn, b 8 hn]

qns :: [Music Pitch]
qns =  [c 0 qn, cs 0 qn, d 0 qn, ds 0 qn, e 0 qn, f 0 qn, fs 0 qn, g 0 qn, gs 0 qn, a 0 qn, as 0 qn, b 0 qn, 
        c 1 qn, cs 1 qn, d 1 qn, ds 1 qn, e 1 qn, f 1 qn, fs 1 qn, g 1 qn, gs 1 qn, a 1 qn, as 1 qn, b 1 qn, 
        c 2 qn, cs 2 qn, d 2 qn, ds 2 qn, e 2 qn, f 2 qn, fs 2 qn, g 2 qn, gs 2 qn, a 2 qn, as 2 qn, b 2 qn, 
        c 3 qn, cs 3 qn, d 3 qn, ds 3 qn, e 3 qn, f 3 qn, fs 3 qn, g 3 qn, gs 3 qn, a 3 qn, as 3 qn, b 3 qn, 
        c 4 qn, cs 4 qn, d 4 qn, ds 4 qn, e 4 qn, f 4 qn, fs 4 qn, g 4 qn, gs 4 qn, a 4 qn, as 4 qn, b 4 qn, 
        c 5 qn, cs 5 qn, d 5 qn, ds 5 qn, e 5 qn, f 5 qn, fs 5 qn, g 5 qn, gs 5 qn, a 5 qn, as 5 qn, b 5 qn, 
        c 6 qn, cs 6 qn, d 6 qn, ds 6 qn, e 6 qn, f 6 qn, fs 6 qn, g 6 qn, gs 6 qn, a 6 qn, as 6 qn, b 6 qn, 
        c 7 qn, cs 7 qn, d 7 qn, ds 7 qn, e 7 qn, f 7 qn, fs 7 qn, g 7 qn, gs 7 qn, a 7 qn, as 7 qn, b 7 qn,  
        c 8 qn, cs 8 qn, d 8 qn, ds 8 qn, e 8 qn, f 8 qn, fs 8 qn, g 8 qn, gs 8 qn, a 8 qn, as 8 qn, b 8 qn]

ens :: [Music Pitch]
ens =  [c 0 en, cs 0 en, d 0 en, ds 0 en, e 0 en, f 0 en, fs 0 en, g 0 en, gs 0 en, a 0 en, as 0 en, b 0 en, 
        c 1 en, cs 1 en, d 1 en, ds 1 en, e 1 en, f 1 en, fs 1 en, g 1 en, gs 1 en, a 1 en, as 1 en, b 1 en, 
        c 2 en, cs 2 en, d 2 en, ds 2 en, e 2 en, f 2 en, fs 2 en, g 2 en, gs 2 en, a 2 en, as 2 en, b 2 en, 
        c 3 en, cs 3 en, d 3 en, ds 3 en, e 3 en, f 3 en, fs 3 en, g 3 en, gs 3 en, a 3 en, as 3 en, b 3 en, 
        c 4 en, cs 4 en, d 4 en, ds 4 en, e 4 en, f 4 en, fs 4 en, g 4 en, gs 4 en, a 4 en, as 4 en, b 4 en, 
        c 5 en, cs 5 en, d 5 en, ds 5 en, e 5 en, f 5 en, fs 5 en, g 5 en, gs 5 en, a 5 en, as 5 en, b 5 en, 
        c 6 en, cs 6 en, d 6 en, ds 6 en, e 6 en, f 6 en, fs 6 en, g 6 en, gs 6 en, a 6 en, as 6 en, b 6 en, 
        c 7 en, cs 7 en, d 7 en, ds 7 en, e 7 en, f 7 en, fs 7 en, g 7 en, gs 7 en, a 7 en, as 7 en, b 7 en,  
        c 8 en, cs 8 en, d 8 en, ds 8 en, e 8 en, f 8 en, fs 8 en, g 8 en, gs 8 en, a 8 en, as 8 en, b 8 en]

sns :: [Music Pitch]
sns =  [c 0 sn, cs 0 sn, d 0 sn, ds 0 sn, e 0 sn, f 0 sn, fs 0 sn, g 0 sn, gs 0 sn, a 0 sn, as 0 sn, b 0 sn, 
        c 1 sn, cs 1 sn, d 1 sn, ds 1 sn, e 1 sn, f 1 sn, fs 1 sn, g 1 sn, gs 1 sn, a 1 sn, as 1 sn, b 1 sn, 
        c 2 sn, cs 2 sn, d 2 sn, ds 2 sn, e 2 sn, f 2 sn, fs 2 sn, g 2 sn, gs 2 sn, a 2 sn, as 2 sn, b 2 sn, 
        c 3 sn, cs 3 sn, d 3 sn, ds 3 sn, e 3 sn, f 3 sn, fs 3 sn, g 3 sn, gs 3 sn, a 3 sn, as 3 sn, b 3 sn, 
        c 4 sn, cs 4 sn, d 4 sn, ds 4 sn, e 4 sn, f 4 sn, fs 4 sn, g 4 sn, gs 4 sn, a 4 sn, as 4 sn, b 4 sn, 
        c 5 sn, cs 5 sn, d 5 sn, ds 5 sn, e 5 sn, f 5 sn, fs 5 sn, g 5 sn, gs 5 sn, a 5 sn, as 5 sn, b 5 sn, 
        c 6 sn, cs 6 sn, d 6 sn, ds 6 sn, e 6 sn, f 6 sn, fs 6 sn, g 6 sn, gs 6 sn, a 6 sn, as 6 sn, b 6 sn, 
        c 7 sn, cs 7 sn, d 7 sn, ds 7 sn, e 7 sn, f 7 sn, fs 7 sn, g 7 sn, gs 7 sn, a 7 sn, as 7 sn, b 7 sn,  
        c 8 sn, cs 8 sn, d 8 sn, ds 8 sn, e 8 sn, f 8 sn, fs 8 sn, g 8 sn, gs 8 sn, a 8 sn, as 8 sn, b 8 en]

tns :: [Music Pitch]
tns =  [c 0 tn, cs 0 tn, d 0 tn, ds 0 tn, e 0 tn, f 0 tn, fs 0 tn, g 0 tn, gs 0 tn, a 0 tn, as 0 tn, b 0 tn, 
        c 1 tn, cs 1 tn, d 1 tn, ds 1 tn, e 1 tn, f 1 tn, fs 1 tn, g 1 tn, gs 1 tn, a 1 tn, as 1 tn, b 1 tn, 
        c 2 tn, cs 2 tn, d 2 tn, ds 2 tn, e 2 tn, f 2 tn, fs 2 tn, g 2 tn, gs 2 tn, a 2 tn, as 2 tn, b 2 tn, 
        c 3 tn, cs 3 tn, d 3 tn, ds 3 tn, e 3 tn, f 3 tn, fs 3 tn, g 3 tn, gs 3 tn, a 3 tn, as 3 tn, b 3 tn, 
        c 4 tn, cs 4 tn, d 4 tn, ds 4 tn, e 4 tn, f 4 tn, fs 4 tn, g 4 tn, gs 4 tn, a 4 tn, as 4 tn, b 4 tn, 
        c 5 tn, cs 5 tn, d 5 tn, ds 5 tn, e 5 tn, f 5 tn, fs 5 tn, g 5 tn, gs 5 tn, a 5 tn, as 5 tn, b 5 tn, 
        c 6 tn, cs 6 tn, d 6 tn, ds 6 tn, e 6 tn, f 6 tn, fs 6 tn, g 6 tn, gs 6 tn, a 6 tn, as 6 tn, b 6 tn, 
        c 7 tn, cs 7 tn, d 7 tn, ds 7 tn, e 7 tn, f 7 tn, fs 7 tn, g 7 tn, gs 7 tn, a 7 tn, as 7 tn, b 7 tn,  
        c 8 tn, cs 8 tn, d 8 tn, ds 8 tn, e 8 tn, f 8 tn, fs 8 tn, g 8 tn, gs 8 tn, a 8 tn, as 8 tn, b 8 tn]
