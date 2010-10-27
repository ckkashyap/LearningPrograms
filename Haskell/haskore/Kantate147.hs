module Haskore.Example.Kantate147 where

{- Kantate 147 by Johann Sebastian Bach -}

import qualified Haskore.Basic.Pitch    as Pitch
import qualified Haskore.Basic.Tempo    as Tempo
import qualified Haskore.Music as Music
import           Haskore.Music (line, chord, (=:=))
import qualified Haskore.Melody   as Melody
import qualified Haskore.Music.GeneralMIDI as MidiMusic
import Haskore.Basic.Duration (qn, (%+), )

import qualified Haskore.Performance.Context as Context
import qualified Haskore.Performance.Default as DefltPf

import qualified Numeric.NonNegative.Wrapper as NonNeg

import qualified Haskore.Interface.MML as MML
-- import qualified Medium.Controlled.List as CtrlMedium
import qualified Medium.Controlled.ContextFreeGrammar as Grammar
import qualified Data.MarkovChain as MarkovChain

import qualified Sound.MIDI.File         as MidiFile
import qualified Haskore.Interface.MIDI.InstrumentMap as InstrumentMap
import qualified Haskore.Interface.MIDI.Write        as WriteMidi
import qualified Sound.MIDI.File.Save         as SaveMidi
import qualified Sound.MIDI.General      as GeneralMidi

import qualified Data.List as List

import           Control.Monad.Trans.State (state, evalState, )
import           System.Random (mkStdGen, split, )


initOctaves :: [Pitch.Octave]
initOctaves = [1, 0, 2, 2]

songMML :: [(String, String, String, String)]
songMML = [
  ("l2g>ge",       "l2p2de",           "l2p2l6g3f#g3a",   "l6p6gab>dcced"),
  ("<b>e<e",       "ge<b",             "b3ab3ge3d",       "dgf#gd<bgab"),
  ("ab>c",         "a>dc",             "e3f#g3de3<b",     ">cdedc<babg"),
  ("df#d",         "c<a>f#",           "a3>da3ga3f#",     "f#gadf#a>c<ba"),
  ("gec",          "g<g>e",            "d3f#g3f#g3a",     "bgab>dcced"),
  ("<b>ed",        "ge<b",             "b3ab3ge3g",       "dgf#gd<bgab"),
  ("cc#d",         ">ced",             "a3f#g3e<a3>c",    "e>dc<bagdgf#"),
  ("<gp3>g6d3<b6", "dp2b3g6",          "<b3>gb3>dg3d",    "gb>dgd<bgb>d"),
  ("g>f#e",        "d<gg",             "l2<g1g",          "l2<b1>c"),
  ("f#ed",         "agf#",             "a1b",             "d1d"),
  ("ef#g",         "gag",              "bag",             "c1<b"),
  ("dp3d6d3d6",    "f#a3a6>d3d6",      "al6d3ef#3g",      "l6adef#aga>c<b"),
  ("<d>p3d6d3d6",  "f#3a6f#3d6<a3>d6", "a3>c<a3f#d3f#",   ">c<af#df#a>c<ba"),
  ("gf#e",         "dde",              "g3dg3f#g3a",      "bgab>dcced"),
  ("b<b>e",        "gd<b",             "b3ag3f#e3g",      "dgf#gd<bgab"),
  ("cd<d",         "l4>c<a>d<b>c<al2", "a3gf#3ga3c",      "e>dc<bagdgf#"),
  ("g>ge",         "b>de",             "<b3>dg3f#g3a",    "gbab>dcced"),
  ("<b>e<e",       "ge<b",             "b3ab3ge3d",       "dgf#gd<bgab"),
  ("ab>c",         "a>dc",             "e3f#g3de3<b",     ">cdedc<babg"),
  ("df#d",         "c<a>f#",           "a3>f#a3ga3f#",    "f#gadf#a>c<ba"),
  ("gec",          "g<g>e",            "d3f#g3f#g3a",     "bgab>dcced"),
  ("<b>ed",        "ge<b",             "b3ab3ge3g",       "dgf#gd<bgab"),
  ("cc#d",         ">ced",             "a3f#g3e<a3>c",    "e>dc<bagdgf#"),
  ("<g>f#e",       "d<gg",             "l2b1>c",          "l2g1g"),
  ("f#ed",         "agf#",             "d1d",             "a1b"),
  ("ef#g",         "gag",              "c1<b",            "bag"),
  ("dp3d6d3d6",    "f#l6a3a>d3d",      "al6d3ef#3g",      "l6ddef#aga>c<b"),
  ("<dp3>d6d3d6",  "f#3af#3d<a3>d",    "a3>c<a3f#d3f#",   ">c<af#df#a>c<ba"),
  ("gf#e",         "l2dde",            "l2b1>c",          "bgab>dcced"),
  ("b<b>e",        "gd<b",             "d1<b",            "dgf#gd<bgab"),
  ("cd<d",         "l4>c<a>d<b>c<a",   "a4b8>c8<ba",      "e>dc<bagdgf#"),
  ("g>ge",         "l2b>de",           "l6g3dg3f#g3a",    "gbab>dcced"),
  ("<b>e<e",       "ge<b",             "b3ab3ge3d",       "dgf#gd<bgab"),
  ("ab>c",         "a>dc",             "e3f#g3de3<b",     ">cdedc<babg"),
  ("df#d",         "c<a>f#",           "a3>da3ga3f#",     "f#gadf#a>c<ba"),
  ("gec",          "g<g>e",            "d3f#g3f#g3a",     "bgab>dcced"),
  ("<b>ed",        "ge<b",             "b3ab3ge3g",       "dgf#gd<bgab"),
  ("cc#d",         ">ced",             "a3f#g3e<a3>c",    "e>dc<bagdgf#"),
  ("<gp3>g6f#3e6", "dp3g6d3e6",        "<b3>gb3>dg3<g",   "gb>dgd<bdb>c#"),
  ("dc<b",         "f#dd",             "l2a1b",           "d<def#ag#g#ba"),
  ("a>a4g4f4e4",   "e<a>a",            ">c1c",            "a>c<b>c<aecde"),
  ("d<b>e",        "aag#",             "<bb4>c8d8<b",     "f>dcd<bg#ef#g#"),
  ("a>fd",         "e<a>f#",           "al6a3g#a3b",      "a>c<b>ceddfe"),
  ("cfe",          "afc",              ">c3<b>c3<af3a",   "eag#aec<ab>c"),
  ("dd#e",         "df#e",             "a3g#a3f#<b3>d",   "fedc<baeag#"),
  ("<a>ab",        "c<ag",             ">l2c1d",          "a>ceap3l2d"),
  (">c<ae",        ">cag",             "e1e",             "l6ecdegfgb-a"),
  ("fdg",          "df#g",             "dd4e8f8d",        "a>c<b>c<afdef"),
  ("cec",          "geg",              "l6c3<g>c3<ge3d",  "egfgec<gab-"),
  ("fdg",          "fag",              "c3ef3ab3>d",      "a>c<b>c<afdef"),
  ("cp3c6<b3>d6",  "gp3d6d3d6",        "c3<g>c3<a>d3<f#", "ecdegf#gba"),
  ("<g>ge",        "dde",              "l2b1>c",          "bgab>dcced"),
  ("<b>e<e",       "ge<b",             "d1d",             "dgf#gd<bgab"),
  ("ab>c",         "a>dc",             "c<b1",            ">cdedc<babg"),
  ("dp3d6d3d6",    "cl6<a3a>d3d",      "l6a3c#d3ef#3g",   "f#def#aga>c<b"),
  ("<dp3>d6d3d6",  "f#3af#3d<a3>d",    "a3>c<a3f#d3f#",   ">c<af#df#a>c<ba"),
  ("gf#e",         "l2dde",            "l2b1>c",          "bgab>dcced"),
  ("b<b>e",        "gd<b",             "d1<b",            "dgf#gd<bgab"),
  ("cd<d",         "l4>c<a>d<b>c<a",   "a4b8>c8<ba",      "e>dc<bagdgf#"),
  ("g1g2",         "l2gp3>g6d3g6",     "gl6<b3>dg3d",     "gb>dgd<bgb>a"),
  ("g1g2",         "dp3g6e3c6",        "<b3g>d3b>c2",     "fd<bgb>ded<a"),
  ("g1g2",         "<ap3>d6<b3>e6",    "c3<ab2b3g",       "f#a>cd<bgegb"),
  ("g1g2",         "<e3a6f#3>a6f#3d6", "a2a3f#d3f#",      ">c<af#df#a>c<ba"),
  ("g>ge",         "dde",              "g3dg3f#g3a",      "bgab>dcced"),
  ("<b>e<e",       "ge<b",             "b3ab3ge3d",       "dgf#gd<bgab"),
  ("ab>c",         "a>d<c",            "e3f#g3de3<b",     ">cdedc<babg"),
  ("df#d",         "c<a>f#",           "a3>da3ga3f#",     "f#gadf#a>c<ba"),
  ("gec",          "g<g>e",            "d3f#g3f#g3a",     "bgab>dcced"),
  ("<b>ed",        "ge<d",             "b3ab3ge3g",       "dgf#gd<bgab"),
  ("cc#d",         "d1d2",             "a3f#g3e<a3>c",    "e>dc<bagdgf#"),
  ("<g1g2",        "p2",               "<b1b2",           "g1g2"),
  ("p1",           "p1",               "p1",              "p1")
  ]


musicTracks :: [[[Melody.T ()]]]
musicTracks =
   let (track0, track1, track2, track3) = List.unzip4 songMML
       trackToMusic tr oct =
          (evalState (mapM (MML.toMusicState) tr) (0, oct))
   in  zipWith trackToMusic [track0, track1, track2, track3]
                            initOctaves

song :: Melody.T ()
song = line (map (chord . map line) (List.transpose musicTracks))


{- Try to reconstruct a structure of the music. -}

grammar :: Grammar.T String Music.Control (Music.Primitive (Melody.Note ()))
grammar =
   let songTrackwise = chord (map (line . concat) musicTracks)
       songConvDurs  = fmap (\(Music.Atom dr at) -> Music.Atom (dr * (3%+4)) at) songTrackwise
   in  Grammar.fromMedium (map (("part"++) . show) [(0 :: Int) ..]) 4
          songConvDurs


{- Try to create new music by reordering the notes using Markov chains. -}

markovChain :: Melody.T ()
markovChain =
   let tracks = map concat musicTracks
       gs     = evalState (sequence (repeat (state split))) (mkStdGen 147)
       chains = zipWith (\track g -> line (MarkovChain.run 3 track 0 g)) tracks gs
   in  chains !! 2 =:= chains !! 3


markovChainMidi :: MidiFile.T
markovChainMidi = toMidi (Music.take 100 markovChain)



----- Player details

cm :: InstrumentMap.ChannelTable MidiMusic.Instrument
cm = [(MidiMusic.ChurchOrgan, MidiMusic.toChannel 1),
      (MidiMusic.Viola,       MidiMusic.toChannel 2)]

context :: Context.T NonNeg.Float Float MidiMusic.Note
context =
   Context.setDur (Tempo.metro 105 qn) $
   DefltPf.context

toMidi :: Melody.T () -> MidiFile.T
toMidi m =
   WriteMidi.fromGMMusic (cm, context,
         MidiMusic.fromMelodyNullAttr MidiMusic.ChurchOrgan m)


midi :: MidiFile.T
midi = toMidi song

main :: IO ()
main = SaveMidi.toFile "test.mid" midi
