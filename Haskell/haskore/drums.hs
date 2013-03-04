import Euterpea

rhythm n = Modify (Instrument Percussion) $
                (line (take (n * 8) (cycle [gs 3 en]))) :=:
                (line (take (n * 8) (cycle [c 3 en, rest en, e 3 en, rest en])))
                

tune = line [wnr] :+:
       line [a 4 dqn, b 4 en, c 5 en, b 4 en, a 4 en, e 4 en ] :+:
       line [a 4 dqn, b 4 en, c 5 en, b 4 en, b 4 en, enr ] :+:
       line [g 4 dqn, a 4 en, b 4 en, a 4 en, g 4 en, d 4 en ] :+:
       line [g 4 dqn, a 4 en, b 4 en, a 4 en, a 4 en, enr]

leftHand = Modify (Instrument PanFlute) $
           line [wnr] :+:
           line [a 4 wn] :+:
           line [a 4 wn] :+:
           line [g 4 wn] :+:
           line [g 4 wn]




music = tune :=: rhythm 5 :=: leftHand

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