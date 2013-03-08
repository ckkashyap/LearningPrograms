import Euterpea

rhythm = repeatM $ phrase [Dyn (Loudness 50)] rhythm'
rhythm' = instrument Percussion $
                      repeatM (phrase [Dyn (Loudness 50)] $ perc SideStick hn) /=:
                      line [
                           perc BassDrum1 qn,
                           perc BassDrum1 qn,
                           perc ElectricSnare qn,

                           --perc BassDrum1 en,
                           enr,
                           perc BassDrum1 en,

                           --perc BassDrum1 qn,
                           qnr,
                           perc BassDrum1 qn,
                           perc ElectricSnare qn,

                           qnr,

                           perc BassDrum1 qn,
                           perc BassDrum1 qn,
                           perc ElectricSnare qn,

                           perc BassDrum1 en,
                           --perc BassDrum1 en,
                           enr,

                           perc BassDrum1 qn,
                           qnr,
                           perc ElectricSnare en,
                           perc BassDrum1 en,
                           perc BassDrum1 qn
                           ]

                      

rhythmHitHat = instrument Percussion $ repeatM (perc PedalHiHat qn)


base = repeatM $ transpose 12 $ phrase [Dyn (Loudness 40)] $ instrument Harpsichord $
           (timesM 2 (line [a 3 qn, c 4 qn])) :+: (timesM 2 (line [a 3 qn, b 3 qn])) :+: (timesM 4 $ a 3 qn) :+: (timesM 2 (line [a 3 qn, b 3 qn]))

chorus = repeatM $ phrase [Dyn (Loudness 30)] $ instrument SynthVoice $
           line [a 4 wn, b 4 wn, e 4 wn, a 4 wn ]

deepVoice = repeatM $ phrase [Dyn (Loudness 10)] $ instrument VoiceOohs $
          chord [a 6 bn, c 7 bn, e 7 bn ]


intro = takeM 12 $ (bnr :+: bnr :+: (touch base)) :=: rhythm :=: deepVoice  :=: chorus

leadTrackPart1 = 
          line [ qnr, e 6 qn, d 6 qn, e 6 en, enr] :+:
          line [ c 6 (qn+en), b 5 (qn+en), a 5 en, enr] :+:
          line [ g 5 (qn+en), a 5 (qn+en), c 6 qn, b 5 hn] :+:
          hnr

leadTrackPart2 =
  leadTrackPart2a :+:
  leadTrackPart2b :+:
  leadTrackPart2c :+:
  leadTrackPart2d :+:
  qnr :+:
  leadTrackPart2e :+:
  
  rest 0
  
  

leadTrackPart2a =  
          line [ a 5 qn, b 5 qn, c 6 qn, a 5 qn, b 5 qn, c 6 qn, e 6 qn, d 6 qn, b 5 bn  ] :+:
          line [ a 5 qn, b 5 qn, c 6 qn, a 5 qn, b 5 qn, c 6 qn, e 6 qn, f 6 qn, d 6 hn  ] :+:
          rest 0

leadTrackPart2b =
          line [c 6 qn, d 6 qn, f 6 en, e 6 (hn+en+qn+hn)] :+:
          line [e 6 qn, c 6 qn, e 6 en, d 6 (hn+en+qn+hn)] :+:
          rest 0
leadTrackPart2c =           
          line [c 6 qn, d 6 qn, f 6 en, e 6 (hn+en+qn+hn)] :+:
          line [e 6 qn, c 6 qn, e 6 en, d 6 (hn+en+qn+hn)] :+:
          rest 0
leadTrackPart2d =
          line [ a 5 hn, e 6 (hn-sn-sfn), d 6 (qn - en - sfn), d 6 (qn+qn+sfn+en-sn-sn + sfn)] :+:
          line [ b 5 (qn-sn-sfn), c 6 (hn-sn-sn+sn+sn+sn), a 5 (qn-en-sfn), a 5 (qn+sn+sfn)] :+:
          rest 0

leadTrackPart2e =
          line [ a 5 hn, e 6 (hn-sn-sfn), d 6 (qn - en - sfn), d 6 (qn+qn+sfn+en-sn-sn + sfn)] :+:
          line [ b 5 (qn-sn-sfn), c 6 (hn-sn-sn+sn+sn+sn), a 5 (qn-en-sfn), a 5 (sn+sfn+sfn+sfn)] :+:
          rest 0


leadTrack = phrase [Dyn (Loudness 80)] $ instrument AcousticGrandPiano $
          timesM 2 leadTrackPart1 :+:
          timesM 1 leadTrackPart2
          


testDrum = rhythm

touch m = m :=: transpose 19 m

leadTrackTouched = touch leadTrack

song = intro :+: ((rhythm :=: deepVoice :=: chorus ) /=: leadTrackTouched) :+: continuation

continuation = takeM 12 $ (touch base) :=: rhythm :=: deepVoice  :=: chorus       

doit = play (tempo (1.45) song)
--doit = play (tempo (1.45) (leadTrack /=: rhythm))


