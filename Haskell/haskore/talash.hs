import Euterpea

rhythm = instrument Percussion $
                      repeatM (perc PedalHiHat qn) :=:
                      repeatM (perc BassDrum1 qn :+: qnr :+: perc ElectricSnare qn :+: (timesM 2 $ perc BassDrum1 qn) :+: qnr :+: perc ElectricSnare qn :+: qnr  )

base = repeatM $ transpose 12 $ phrase [Dyn (Loudness 40)] $ instrument Harpsichord $
           (timesM 2 (line [a 3 qn, c 4 qn])) :+: (timesM 2 (line [a 3 qn, b 3 qn])) :+: (timesM 4 $ a 3 qn) :+: (timesM 2 (line [a 3 qn, b 3 qn]))

chorus = repeatM $ phrase [Dyn (Loudness 30)] $ instrument SynthVoice $
           line [a 4 wn, b 4 wn]

deepVoice = repeatM $ phrase [Dyn (Loudness 10)] $ instrument VoiceOohs $
          chord [a 6 bn, c 7 bn, e 7 bn ]


intro = takeM 12 $ (bnr :+: bnr :+: (touch base)) :=: rhythm :=: deepVoice  :=: chorus

leadTrackPart1 = 
          line [ qnr, e 6 qn, d 6 qn, e 6 en, enr] :+:
          line [ c 6 (qn+en), b 5 (qn+en), a 5 en, enr] :+:
          line [ g 5 (qn+en), a 5 (qn+en), c 6 qn, b 5 hn] :+:
          hnr

leadTrack = phrase [Dyn (Loudness 80)] $ instrument AcousticGrandPiano $
          timesM 2 leadTrackPart1


touch m = m :=: transpose 19 m

leadTrackTouched = touch leadTrack

song = intro :+: ((rhythm :=: deepVoice :=: chorus ) /=: leadTrackTouched)

doit = play (tempo (1.45) song)
--doit = play (tempo (1.45) (leadTrack /=: rhythm))


