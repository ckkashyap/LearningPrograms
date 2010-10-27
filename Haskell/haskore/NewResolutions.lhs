% GHC-6.4.1 runs out of memory with optimization.
% Unfortunately we cannot override Cabal's option here,
% so you have to configure with --disable-optimization
% {-# OPTIONS_GHC -Onot #-}

New Resolutions by Jean-Luc Ponty, Scott O'Neil, and John Garvin

> module Haskore.Example.NewResolutions where

> import qualified Haskore.Basic.Pitch as Pitch
> import qualified Haskore.Basic.Tempo as Tempo
> import qualified Haskore.Interface.MIDI.Write as WriteMidi
> import qualified Sound.MIDI.File.Save    as SaveMidi
> import qualified Sound.MIDI.File         as MidiFile

> import qualified Haskore.Performance.Context as Context
> import qualified Haskore.Performance.Fancy   as FancyPf

> import Haskore.Basic.Duration((%+))
> import Haskore.Basic.Pitch
> import Haskore.Basic.Interval as Interval
> import qualified Haskore.Music as Music
> import Haskore.Melody            as Melody
> import Haskore.Melody.Standard   as StdMelody
> import Haskore.Music.GeneralMIDI as MidiMusic
> import qualified Haskore.Music.Rhythmic  as RhyMusic

> import qualified Data.List as List

> import qualified Numeric.NonNegative.Wrapper as NonNeg
> import qualified Data.Accessor.Basic as Accessor


> piano, marimba, xylo, vib, glock :: MidiMusic.Instr
> piano   = MidiMusic.AcousticGrandPiano
> marimba = MidiMusic.Marimba
> xylo    = MidiMusic.Xylophone
> vib     = MidiMusic.Vibraphone
> glock   = MidiMusic.Glockenspiel

> pattern, melPattern,
>  melody1, bellPart, vibesLine, vibesPart,
>  melody2, vibeLine3, vibePart3,
>  melody3, endRun :: StdMelody.T
> part1, part2, part3, bridge, ending, harmony3 :: MidiMusic.T

> comp2 :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
> comp2 func = ((func .) .)

% comp2 func1 func0 = curry (func1 . uncurry func0)

> arpeggio :: [Int] -> Pitch.T -> Dur -> StdMelody.T
> arpeggio trs p d' = line (map (\tr -> note (Pitch.transpose tr p) d' na) trs)

> minArpegUp, minArpegDown, majArpegDown, six3ArpegDown
>    :: Pitch.T -> Dur -> StdMelody.T
> minArpegUp    = arpeggio [unison, minorThird, fifth, octave]
> minArpegDown  = arpeggio [octave, fifth, minorThird, unison]
> majArpegDown  = arpeggio [octave, fifth, majorThird, unison]
> six3ArpegDown = arpeggio [octave, majorSixth, majorThird, unison]

> pattern = minArpegUp (5,D) sn
>       +:+ minArpegDown (5,C) sn
>       +:+ minArpegUp (4,A) sn
>       +:+ minArpegDown (4,G) sn
>       +:+ minArpegUp (4,F) sn
>       +:+ d 5 sn na +:+ a 4 sn na +:+ f 4 sn na +:+ a 4 sn na

> melPattern = d 6 en na +:+ c 6 en na +:+ d 6 en na
>          +:+ snr
>          +:+ a 5 en na +:+ g 5 en na +:+ a 5 en na

> melody1 = melPattern +:+ enr +:+ d 5 sn na
>       +:+ f 5 sn na +:+ g 5 en na +:+ f 5 sn na +:+ d 5 en na +:+ c 5 en na
>       +:+ d 5 en na +:+ melPattern +:+ d 5 sn na
>       +:+ f 5 sn na +:+ f 5 sn na +:+ g 5 sn na +:+ f 5 sn na
>       +:+ d 5 sn na +:+ c 5 en na +:+ d 5 den na
>       +:+ melPattern +:+ d 5 sn na
>       +:+ f 5 sn na +:+ g 5 sn na +:+ f 5 sn na +:+ d 5 en na
>       +:+ c 5 sn na +:+ d 5 en na
>       +:+ d 6 en na +:+ c 6 en na +:+ d 6 den na +:+ c 6 en na
>       +:+ a 5 en na +:+ c 6 en na +:+ a 5 sn na +:+ g 5 en na
>       +:+ f 5 en na +:+ af 5 en na
>       +:+ g 5 sn na +:+ f 5 sn na +:+ d 5 sn na +:+ c 5 sn na
> -- last note removed to make fit with pattern

> bellPart = d 7 en na +:+ f 7 en na +:+ c 7 en na +:+ d 7 en na
>        +:+ a 6 en na +:+ c 7 en na +:+ g 6 en na +:+ a 6 en na
>        +:+ f 6 en na +:+ g 6 en na
>        +:+ d 6 sn na +:+ f 6 sn na +:+ a 6 sn na +:+ c 7 sn na

> vibesLine = d 5 qn na +:+ c 5 qn na +:+ a 4 qn na
>         +:+ g 4 qn na +:+ f 4 qn na +:+ d 4 qn na
> vibesPart = vibesLine =:= Music.transpose 12 vibesLine

> cMajorScale, gMajorScale, dPentMinScale :: [Pitch.T]
> cMajorScale = [(0,C), (0,D), (0,E), (0,F), (0,G), (0,A), (0,B)]
> gMajorScale = [(0,G), (0,A), (0,B), (1,C), (1,D), (1,E), (1,Fs)]
> dPentMinScale = [(0,D), (0,F), (0,G), (0,A), (1,C)]

> prevNote, nextNote :: [Pitch.T] -> Pitch.T -> Pitch.T
> prevNote []         _       = error ("Scale empty")
> prevNote [_]        _       = error ("Note not found in scale")
> prevNote ((n,y):ys) (oct,p) | y == p = let (m,x) = last ys
>                                        in (oct + m - n - 1, x)
> prevNote ((m,x):(n,y):xys) (oct,p) | y == p    = (oct + m - n, x)
>                                    | otherwise = prevNote ((n,y):xys) (oct,p)

> nextNote scale n = nextNote' (head scale) scale n
> nextNote' :: Pitch.T -> [Pitch.T] -> Pitch.T -> Pitch.T
> nextNote' _ [] _ = error ("Scale empty")
> nextNote' (fstO,fstP) [(m,x)]           (oct,p)
>                                       | x == p    = (oct - m + fstO + 1, fstP)
>                                       | otherwise = error ("Note not found in scale")
> nextNote' fst'        ((m,x):(n,y):xys) (oct,p)
>                                       | x == p    = (oct - m + n, y)
>                                       | otherwise = nextNote' fst' ((n,y):xys) (oct,p)

> back2Note :: [Pitch.T] -> Pitch.T -> Pitch.T
> back2Note s = prevNote s . prevNote s

> nextNR, prevNR, back2NR :: Pitch.T -> Pitch.T
> nextNR = nextNote dPentMinScale
> prevNR = prevNote dPentMinScale
> back2NR = back2Note dPentMinScale

> makeSN, diddle :: Pitch.T -> StdMelody.T
> makeSN p = note p sn na
> diddle p = line $ snr : map makeSN [p, prevNR p, p]

> melody2 = d 6 sn na +:+ d 6 en na +:+ c 6 en na +:+ d 6 sn na +:+ c 6 en na
>       +:+ a 5 en na +:+ g 5 sn na +:+ f 5 sn na
>       +:+ g 5 sn na +:+ f 5 sn na +:+ d 5 sn na +:+ f 5 sn na
>       +:+ diddle (5,D) +:+ diddle (5,C)
>       +:+ diddle (6,D) +:+ diddle (6,C) +:+ diddle (5,A)
>       +:+ diddle (5,G) +:+ diddle (5,F) +:+ diddle (5,D)
>       +:+ snr +:+ d 6 en na +:+ c 6 en na +:+ d 6 den na
>       +:+ c 6 en na +:+ a 5 en na +:+ g 5 den na
>       +:+ f 5 en na +:+ g 5 en na +:+ f 5 sn na
>       +:+ g 5 sn na +:+ f 5 sn na +:+ d 5 sn na +:+ c 5 sn na
>       +:+ d 5 den na +:+ d 6 en na +:+ c 6 den na +:+ a 5 en na +:+ g 5 den na
>       +:+ f 5 en na +:+ d 5 den na +:+ c 5 en na +:+ d 5 qn na

> part1 = MidiMusic.fromStdMelody marimba (loudness1 0.7 pattern)
>         +:+
>         MidiMusic.fromStdMelody xylo    (loudness1 1.2 melody1)
>     =:= MidiMusic.fromStdMelody marimba (loudness1 0.7 (Music.replicate 4 pattern))
> bridge = MidiMusic.fromStdMelody xylo    (d 5 hn (Accessor.set velocity1 1.2 na))
>      =:= (Music.replicate 2 $
>          MidiMusic.fromStdMelody marimba (loudness1 0.6 (Music.transpose (-12) bellPart))
>      =:= MidiMusic.fromStdMelody vib     (loudness1 0.4 vibesPart)
>      =:= MidiMusic.fromStdMelody glock   (loudness1 0.8 bellPart))
> part2 = MidiMusic.fromStdMelody xylo    (loudness1 1.2 melody2)
>     =:= MidiMusic.fromStdMelody marimba (loudness1 0.7 (Music.replicate 3 pattern
>                                                  +:+ minArpegUp   (5,D) sn
>                                                  +:+ minArpegDown (5,C) sn
>                                                  +:+ minArpegUp   (4,A) sn
>                                                  +:+ minArpegDown (4,G) sn
>                                                  +:+ minArpegUp   (4,F) sn
>                                                  +:+ d 5 sn na))
>     =:= Music.replicate 4 (MidiMusic.fromStdMelody vib (loudness1 0.4 vibesPart))

> run1, run2, run3 :: Pitch.T -> Dur -> StdMelody.T
> run1 = arpeggio [unison, minorThird, fifth,
>                  minorSeventh, octave, octaveMinorThird,
>                  octaveFifth, octaveMinorThird, octave,
>                  minorSeventh, fifth, minorThird]

> part3Pattern :: (Num t) =>
>                 ((t, Pitch.Class) -> Dur -> StdMelody.T) -> MidiMusic.T
> part3Pattern el = MidiMusic.fromStdMelody piano $
>     el (4,D) sn +:+ el (4,C) sn +:+ el (4,D) sn +:+ el (4,F) sn

> run2 = Music.replicate 2 `comp2`
>           arpeggio [fifth, minorSeventh, octave,
>                     octaveMinorThird, octave, minorSeventh]

> run3 = Music.replicate 3 `comp2`
>           arpeggio [octaveMinorThird, octave, minorSeventh, fifth]

> vibeLine3 = let el p = arpeggio [octave, fifth, minorSeventh, octave] p den
>             in el (4,D) +:+ el (4,C) +:+ el (4,D)
>                +:+ f 5 den na +:+ c 5 den na
>                +:+ ef 5 en na +:+ f 5 en na +:+ af 5 en na
> vibePart3 = vibeLine3 =:= Music.transpose 12 vibeLine3

> melody3 = a 5 (11%+16) na +:+ f 6 sn na
>       +:+ ef 6 en na +:+ d 6 en na +:+ c 6 en na +:+ g 5 dqn na
>       +:+ Music.replicate 3 (a 5 sn na +:+ f 6 en na) +:+ a 5 en na
>       +:+ f 6 en na +:+ af 5 en na +:+ f 6 en na +:+ af 5 en na
>       +:+ minArpegDown (5,F) sn +:+ snr
>       +:+ majArpegDown (5,F) sn +:+ snr
>       +:+ six3ArpegDown (5,F) sn +:+ snr +:+ f 6 sn na +:+ d 6 sn na
>       +:+ ef 6 sn na +:+ d 6 sn na +:+ c 6 sn na +:+ g 5 sn na +:+ snr
>       +:+ majArpegDown (5,Ef) sn +:+ snr +:+ ef 6 sn na +:+ c 6 sn na
>       +:+ majArpegDown (5,F) sn +:+ snr
>       +:+ six3ArpegDown (5,F) sn +:+ snr +:+ f 6 sn na +:+ d 6 sn na
>       +:+ minArpegDown (5,F) sn +:+ snr
>       +:+ minArpegDown (5,F) sn +:+ af 5 sn na +:+ c 6 sn na +:+ f 6 sn na
>       +:+ line (map (Music.replicate 2) [f 6 sn na, d 6 sn na, c 6 sn na,
>                                a 5 sn na, g 5 sn na, f 5 sn na])
>       +:+ ef 5 sn na +:+ f 5 sn na +:+ g 5 sn na +:+ bf 5 sn na
>       +:+ c 6 sn na +:+ d 6 sn na +:+ ef 6 sn na +:+ d 6 sn na
>       +:+ c 6 sn na +:+ bf 5 sn na +:+ a 5 sn na +:+ g 5 sn na
>       +:+ Music.replicate 4 (a 5 sn na +:+ a 5 sn na +:+ g 5 sn na)
>       +:+ Music.replicate 2 (af 5 sn na +:+ af 5 sn na +:+ g 5 sn na)
>       +:+ Music.replicate 2 (af 5 sn na +:+ g 5 sn na +:+ f 5 sn na)
>       +:+ a 5 dqn na
>       +:+ f 6 sn na +:+ d 6 sn na +:+ c 6 sn na
>       +:+ a 5 sn na +:+ g 5 sn na +:+ f 5 sn na
>       +:+ g 5 sn na +:+ bf 5 sn na +:+ ef 6 dqn na
>       +:+ bf 6 den na +:+ bf 6 sn na
>       +:+ a 6 en na +:+ a 6 sn na +:+ g 6 en na +:+ g 6 sn na
>       +:+ f 6 den na +:+ a 5 sn na +:+ c 6 sn na +:+ d 6 sn na
>       +:+ f 6 den na +:+ f 6 sn na +:+ d 6 sn na +:+ c 6 sn na
>       +:+ af 5 sn na +:+ af 5 sn na +:+ g 5 sn na
>       +:+ f 5 sn na +:+ d 5 sn na +:+ c 5 sn na

> harmony3 = loudness1 0.6 (part3Pattern run1
>                                    =:= part3Pattern run2
>                                    =:= Music.transpose 12 (part3Pattern run3))
>        =:= loudness1 0.5 (MidiMusic.fromStdMelody vib vibePart3)

> part3 = loudness1 0.6 (part3Pattern run1)
>     +:+ (loudness1 0.6 (part3Pattern run1)
>      =:= loudness1 0.9 (part3Pattern run2))
>     +:+ (loudness1 0.6 ((part3Pattern run1)
>      =:= (part3Pattern run2))
>      =:= loudness1 1.0 (Music.transpose 12 (part3Pattern run3)))
>     +:+ loudness1 0.6 (part3Pattern run1
>                                    =:= part3Pattern run2
>                                    =:= Music.transpose 12 (part3Pattern run3))
>      =:= loudness1 0.7 (MidiMusic.fromStdMelody vib vibePart3)
>     +:+ (Music.replicate 4 harmony3 =:=
>          loudness1 1.0 (MidiMusic.fromStdMelody xylo melody3 =:=
>                        MidiMusic.fromStdMelody marimba melody3))

> all3Insts :: StdMelody.T -> MidiMusic.T
> all3Insts m = chord [MidiMusic.fromStdMelody marimba m,
>                      MidiMusic.fromStdMelody xylo    m,
>                      MidiMusic.fromStdMelody vib     m]

> endEl :: Pitch.T -> StdMelody.T
> endEl p = line $ map makeSN [p, back2NR p, prevNR p, p]

> endRun = line $ map endEl $ List.take 10 $ iterate nextNR (5,D)

> ending = all3Insts $
>       d 5 qn na
>   +:+ loudness1 1.2 (endRun +:+ d 7 sn na)



> song :: MidiMusic.T
> song = Music.transpose (-48) $ line [part1, bridge, part2, part3, ending]
>
> -- context :: Context.T NonNeg.Float Float MidiMusic.Note -- rejected by Hugs
> context :: Context.T NonNeg.Float Float (RhyMusic.Note MidiMusic.Drum MidiMusic.Instr)
> context =
>    Context.setDur (Tempo.metro 120 qn) $
>    FancyPf.context
>
> midi :: MidiFile.T
> midi = WriteMidi.fromGMMusicAuto (context, song)
>
> main :: IO ()
> main = SaveMidi.toFile "newresolutions.mid" midi
