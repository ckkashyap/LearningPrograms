import Euterpea


rhythm1 = instrument Percussion $ phrase [(Dyn (Loudness 30))] $ 
       repeatM (perc PedalHiHat qn)

bA = perc AcousticBassDrum qn
bB = perc AcousticSnare qn
bC = perc BassDrum1 qn
bD = perc HandClap qn

drums = instrument Percussion $
      line [bA, bB]
      :=:  line [perc AcousticBassDrum hn, bD]
      :=: line [perc BassDrum1 en, perc BassDrum1 en]

rhythm2  = drums 

rhythm = rhythm1 /=: drums



piano = instrument ElectricGrandPiano $
      line [e 5 (en), d 5 sn, e 5 sn, d 5 (sn), a 4 sn, a 4 sn]

aaah = instrument ChoirAahs $
     chord [ c 4 bn, c 3 bn]


music = (transpose 18 $ piano :+: aaah )
        /=: rhythm


doit = play $ tempo (0.8) music