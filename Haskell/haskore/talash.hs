import Euterpea

rhythm = instrument Percussion $
                      repeatM (perc PedalHiHat qn) :=:
                      repeatM (perc BassDrum1 qn :+: qnr :+: perc ElectricSnare qn :+: (timesM 2 $ perc BassDrum1 qn) :+: qnr :+: perc ElectricSnare qn :+: qnr  )
                


base = repeatM $ phrase [Dyn (Loudness 60)] $ instrument Harpsichord $
           (timesM 2 (line [a 3 qn, c 4 qn])) :+: (timesM 2 (line [a 3 qn, b 3 qn])) :+: (timesM 4 $ a 3 qn) :+: (timesM 2 (line [a 3 qn, b 3 qn]))

chorus = repeatM $ phrase [Dyn (Loudness 30)] $ instrument SynthVoice $
           line [a 4 wn, b 4 wn]

deepVoice = repeatM $ phrase [Dyn (Loudness 10)] $ instrument VoiceOohs $
          chord [a 6 bn, c 7 bn, e 7 bn ]


music = (bnr :+: bnr :+: (transpose 12 base)) :=: rhythm :=: deepVoice  :=: chorus


intro = takeM 12 music

leadTrack = bnr

song = intro :+: ((rhythm :=: deepVoice :=: chorus) /=: leadTrack)

doit = play (tempo (3/2) song)



