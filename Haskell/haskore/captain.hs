import Euterpea


piano = instrument ElectricGrandPiano $
      line [e 5 (en), d 5 sn, e 5 sn, d 5 (sn), a 4 en]

aaah = instrument ChoirAahs $
     line [ c 4 hn]


m = tempo (0.5) $ transpose (18) $ piano :+: aaah