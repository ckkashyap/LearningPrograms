import Euterpea

rhythm = repeatM $ phrase [Dyn (Loudness 50)] rhythm'
rhythm' = instrument Percussion $
                      repeatM (phrase [Dyn (Loudness 50)] $ perc SideStick en) /=:
                      line [enr
                           ]

                      

tune = 
          line [ a 4 en, c 5 en, e 5 en, d 5 en, e 5 en, c 5 en, e 5 en, c 5 en ] :+:
          hnr


doit = play (tempo (1) (tune /=: rhythm))
--doit = play (tempo (1.45) (leadTrack /=: rhythm))


