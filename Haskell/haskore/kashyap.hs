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

cMajScale = Modify (Tempo 2)
            (line [c 4 en, d 4 en, e 4 en, f 4 en,
                   g 4 en, a 4 en, b 4 en, c 5 en])

cms' = line [c 4 en, d 4 en, e 4 en, f 4 en,
             g 4 en, a 4 en, b 4 en, c 5 en]

cms = cMajScale




t1 = play (Modify (Instrument Percussion)
      (Modify (Phrase [Art (Staccato (1/10))]) cms :+:
       cms                             :+:
       Modify (Phrase [Art (Legato  (11/10))]) cms    ))


