# markov_haskell
Two projects exploring markov chains in haskell, human speech and midi (classical piano) 

The first, unfortunately called "Trump.hs," started as a Python tutorial and ended up being the first program I ever wrote in haskell. It was initially a markov speech generator based on Trump's speeches circa 2016, but eventually grew to a generator that could train on various public domain sources such as James Joyce, Virginia Woolf, Shakespeare, the Bible, etc. 

The second, also dumbly called "MarkovMidi.hs" is an experiment in markov chain generation using midi files from European classical composers in the range of Bach to Debussy. My second project in Haskell, it too is a bit rough, using some less-than-best-practices to say the least. The music these chains generate is also rather unlistenable but an interesting experiment using haskell and Euterpea, nonetheless.  

To run you need a bunch of libraries like Euterpea and Sound, but building is just `stack build` and then running is `stack exec trump` or `stack exec markovmidi`

If you run the trump module you'll be prompted for a voice like "trump," "woolf," "yeats," etc. If you run the midi module you should probably just enter "all" which will train on all the composers, since training on single composer is pretty uninteresting.  
