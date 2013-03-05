import Euterpea

rhythm = instrument Percussion $
                      repeatM (_tsh) :=:
                      repeatM (_dhugh :+: rest en :+: _dhush :+: rest en)
                      where
                        _tsh = perc PedalHiHat en
                        _dhugh = perc BassDrum1 en
                        _dhush = perc ElectricSnare en
                
tuneFirstPart f oct = f $
       line [a oct dqn, b oct en, c (oct+1) en, b oct en, a oct en, e oct en ] :+:
       line [a oct dqn, b oct en, c (oct+1) en, b oct en, b oct en, enr ] :+:
       line [g oct dqn, a oct en, b oct en, a oct en, g oct en, d oct en ] :+:
       line [g oct dqn, a oct en, b oct en, a oct en, a oct en, enr] :+:
       rest 0
   
tuneSecondPart f oct = f $
       line [a oct dqn, b oct en, c (oct+1) en, b oct en, a oct en, e oct en ] :+:     
       line [a oct dqn, b oct en, c (oct+1) en, c (oct+1) en, b oct en, enr ] :+:
       line [g oct dqn, a oct en, b oct en, a oct en, g oct en, d oct en ] :+:
       line [g oct dqn, b oct en, c (oct+1) en, b oct en, a oct sn, b oct sn, a oct sn, snr] :+:
       rest 0

tune f oct = (tuneFirstPart f oct) :+: (tuneSecondPart f oct)
     
t1 = tuneFirstPart (phrase [Dyn (Loudness 50)] . (instrument Whistle)) 7
t2' i = tuneSecondPart (phrase [Dyn (Loudness 50)] . (instrument i)) 7
t2 = (t2' Whistle) :=: (t2' TubularBells)

base = repeatM $ phrase [Dyn (Loudness 80)] $ instrument ElectricBassPicked $
           timesM 4 (line [a 2 en, a 2 en, e 3 en, a 2 en]) :+:
           timesM 4 (line [g 2 en, g 2 en, d 3 en, g 2 en ])

music = (bnr :+: rhythm) /=: (rest 4 :+: t1 :+: t2) /=: base

doit = play music



{- documentation

Euterpea/Euterpea/Music/Note/Music.hs

data InstrumentName =
     AcousticGrandPiano     | BrightAcousticPiano    | ElectricGrandPiano
  |  HonkyTonkPiano         | RhodesPiano            | ChorusedPiano
  |  Harpsichord            | Clavinet               | Celesta 
  |  Glockenspiel           | MusicBox               | Vibraphone  
  |  Marimba                | Xylophone              | TubularBells
  |  Dulcimer               | HammondOrgan           | PercussiveOrgan 
  |  RockOrgan              | ChurchOrgan            | ReedOrgan
  |  Accordion              | Harmonica              | TangoAccordion
  |  AcousticGuitarNylon    | AcousticGuitarSteel    | ElectricGuitarJazz
  |  ElectricGuitarClean    | ElectricGuitarMuted    | OverdrivenGuitar
  |  DistortionGuitar       | GuitarHarmonics        | AcousticBass
  |  ElectricBassFingered   | ElectricBassPicked     | FretlessBass
  |  SlapBass1              | SlapBass2              | SynthBass1   
  |  SynthBass2             | Violin                 | Viola  
  |  Cello                  | Contrabass             | TremoloStrings
  |  PizzicatoStrings       | OrchestralHarp         | Timpani
  |  StringEnsemble1        | StringEnsemble2        | SynthStrings1
  |  SynthStrings2          | ChoirAahs              | VoiceOohs
  |  SynthVoice             | OrchestraHit           | Trumpet
  |  Trombone               | Tuba                   | MutedTrumpet
  |  FrenchHorn             | BrassSection           | SynthBrass1
  |  SynthBrass2            | SopranoSax             | AltoSax 
  |  TenorSax               | BaritoneSax            | Oboe  
  |  Bassoon                | EnglishHorn            | Clarinet
  |  Piccolo                | Flute                  | Recorder
  |  PanFlute               | BlownBottle            | Shakuhachi
  |  Whistle                | Ocarina                | Lead1Square
  |  Lead2Sawtooth          | Lead3Calliope          | Lead4Chiff
  |  Lead5Charang           | Lead6Voice             | Lead7Fifths
  |  Lead8BassLead          | Pad1NewAge             | Pad2Warm
  |  Pad3Polysynth          | Pad4Choir              | Pad5Bowed
  |  Pad6Metallic           | Pad7Halo               | Pad8Sweep
  |  FX1Train               | FX2Soundtrack          | FX3Crystal
  |  FX4Atmosphere          | FX5Brightness          | FX6Goblins
  |  FX7Echoes              | FX8SciFi               | Sitar
  |  Banjo                  | Shamisen               | Koto
  |  Kalimba                | Bagpipe                | Fiddle 
  |  Shanai                 | TinkleBell             | Agogo  
  |  SteelDrums             | Woodblock              | TaikoDrum
  |  MelodicDrum            | SynthDrum              | ReverseCymbal
  |  GuitarFretNoise        | BreathNoise            | Seashore
  |  BirdTweet              | TelephoneRing          | Helicopter
  |  Applause               | Gunshot                | Percussion
  |  Custom String
  deriving (Show, Eq, Ord)



-}