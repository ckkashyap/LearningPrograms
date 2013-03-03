import Euterpea


drums a b = Modify (Instrument Percussion)
                  (line (map (\p-> Prim $ Note sn (pitch p)) [a..b]))
t11 a b = play (drums a b)


testDrums = t11 25 100
