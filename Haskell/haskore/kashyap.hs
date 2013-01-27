{-# LANGUAGE Arrows #-}
import Euterpea

list = ["a", "b", "c"]
numbers = [0..10]
m1 i l = map (\v -> v ++ show i) l
output = map f numbers
	where
		f n = m1 n list

cMajorScale = [c, d, e, f, g, a, b]
octaves = [0..10]

something =  line $ map f [0..10]
	where
		f n = g n cMajorScale
		g n l = line $ map (h n) l
		h n v = v n en




saregama = line [ c 4 en, d 4 en, e 4 en, f 4 en]

cMajScale = Modify (Tempo 1)
            (line [c 4 en, d 4 en, e 4 en, f 4 en,
                   g 4 en, a 4 en, b 4 en, c 5 en])

cms' = line [c 4 en, d 4 en, e 4 en, f 4 en,
             g 4 en, a 4 en, b 4 en, c 5 en]

cms = cMajScale


reedyWav = tableSinesN 1024 [0.4, 0.3, 0.35, 0.5, 0.1, 0.2, 0.15, 
                           0.0, 0.02, 0.05, 0.03]

reed :: Instr (Stereo AudRate)
reed dur pch vol params = 
    let reedy = osc reedyWav 0
        freq  = apToHz pch
        vel   = fromIntegral vol / 127 / 3
        env   = envLineSeg [0, 1, 0.8, 0.6, 0.7, 0.6, 0] (replicate 6 (fromRational dur/6))
    in proc _ -> do
      amp <- env -< ()
      r1 <- reedy -< freq
      r2 <- reedy -< freq + (0.023 * freq)
      r3 <- reedy -< freq + (0.019 * freq)
      let [a1, a2, a3] = map (* (amp * vel)) [r1, r2, r3]
      let rleft = a1 * 0.5 + a2 * 0.44 * 0.35 + a3 * 0.26 * 0.65
          rright = a1 * 0.5 + a2 * 0.44 * 0.65 + a3 * 0.26 * 0.35
      outA -< (rleft, rright)

saw = tableSinesN 4096 [1, 0.5, 0.333, 0.25, 0.2, 0.166, 0.142, 0.125, 
                        0.111, 0.1, 0.09, 0.083, 0.076, 0.071, 0.066, 0.062]

plk :: Instr (Stereo AudRate)
plk dur pch vol params = 
    let vel  = fromIntegral vol / 127 / 3
        freq = apToHz pch
        sf   = pluck saw freq SimpleAveraging
    in proc _ -> do
         a <- sf -< freq
         outA -< (a * vel * 0.4, a * vel * 0.6)

myBass, myReed :: InstrumentName
myBass = Custom "pluck-like"
myReed = Custom "reed-like"

myMap :: InstrMap (Stereo AudRate)
myMap = [(myBass, plk), (myReed, reed)]

bass   = mMap (\p-> (p, 40 :: Volume)) $ instrument myBass cms
melody = mMap (\p-> (p,100 :: Volume)) $ instrument myReed cms

childSong6 :: Music (Pitch, Volume)
childSong6 = tempo 1.5 (bass :=: melody)

recordSong = uncurry (outFile "song.wav") (renderSF childSong6 myMap)

t1 = play (Modify (Instrument Percussion)
      (Modify (Phrase [Art (Staccato (1/10))]) cms :+:
       cms                             :+:
       Modify (Phrase [Art (Legato  (11/10))]) cms    ))





drums a b = Modify (Instrument Percussion)
                  (line (map (\p-> Prim $ Note sn (pitch p)) [a..b]))
t11 a b = play (drums a b)


testDrums = t11 25 100
