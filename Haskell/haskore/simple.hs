import qualified Haskore.Basic.Pitch as Pitch
import qualified Haskore.Basic.Tempo as Tempo
import qualified Haskore.Interface.MIDI.Write as WriteMidi
import qualified Sound.MIDI.File.Save    as SaveMidi
import qualified Sound.MIDI.File         as MidiFile

import qualified Haskore.Performance.Context as Context
import qualified Haskore.Performance.Fancy   as FancyPf

import Haskore.Basic.Duration((%+))
import Haskore.Basic.Pitch
import Haskore.Basic.Interval as Interval
import qualified Haskore.Music as Music
import Haskore.Melody            as Melody
import Haskore.Melody.Standard   as StdMelody
import Haskore.Music.GeneralMIDI as MidiMusic
import qualified Haskore.Music.Rhythmic  as RhyMusic

import qualified Data.List as List

import qualified Numeric.NonNegative.Wrapper as NonNeg
import qualified Data.Accessor.Basic as Accessor


piano, marimba, xylo, vib, glock, guitar :: MidiMusic.Instr
piano   = MidiMusic.AcousticGrandPiano
marimba = MidiMusic.Marimba
xylo    = MidiMusic.Xylophone
vib     = MidiMusic.Vibraphone
glock   = MidiMusic.Glockenspiel
guitar  = MidiMusic.AcousticGuitarSteel

context :: Context.T NonNeg.Float Float (RhyMusic.Note MidiMusic.Drum MidiMusic.Instr)
context =
   Context.setDur (Tempo.metro 120 qn) $
   FancyPf.context

--melPattern = c 3 bn na +:+ e 3 bn na

--melPattern = chord [c 3 qn na ,  e 3 qn na, g 3 qn na]

melPattern = line [c1 en, c1 qn, c1 qn, c1 qn, c1 en,  c2 qn]

c1 drn = chord [a 2 drn na, c 3 drn na, e 3 drn na]
c2 drn = chord [g 2 drn na, b 3 drn na, d 3 drn na]


         -- +:+ c 6 en na +:+ d 6 en na
         -- +:+ snr
         -- +:+ a 5 en na +:+ g 5 en na +:+ a 5 en na


cMaj = [ n 4 qn [] | n <- [c,e,g] ] -- octave 4, quarter notes

xxx = MidiMusic.fromStdMelody guitar melPattern
--xxx = MidiMusic.fromStdMelody piano (loudness1 0.7 melPattern)

midi :: MidiFile.T
midi = WriteMidi.fromGMMusicAuto (context, xxx)

main :: IO ()
main = SaveMidi.toFile "newresolutions.mid" midi

