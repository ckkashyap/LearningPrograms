import Euterpea

rhythm = instrument Percussion $
                      repeatM (perc PedalHiHat qn) :=:
                      repeatM (perc BassDrum1 qn :+: qnr :+: perc ElectricSnare qn :+: (timesM 2 $ perc BassDrum1 qn) :+: qnr :+: perc ElectricSnare qn :+: qnr  )
                


base = repeatM $ phrase [Dyn (Loudness 80)] $ instrument AcousticGrandPiano $
           (timesM 2 (line [a 3 qn, c 4 qn])) :+: (timesM 2 (line [a 3 qn, b 3 qn])) :+: (timesM 4 $ a 3 qn) :+: (timesM 2 (line [a 3 qn, b 3 qn]))

chorus = repeatM $ phrase [Dyn (Loudness 100)] $ instrument SynthVoice $
           line [a 4 wn, g 4 wn]

deepVoice = repeatM $ phrase [Dyn (Loudness 100)] $ instrument SynthStrings1 $
          chord [a 1 bn, c 2 bn, e 2 bn ]


music = (bnr :+: (base :=: (transpose 3 base))) :=: rhythm :=: deepVoice 

doit = play music



