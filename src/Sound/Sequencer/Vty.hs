{-# LANGUAGE RecordWildCards #-}

module Sound.Sequencer.Vty
  ( run
  )
where

import Control.Monad (when)
import Data.Char (intToDigit)
import Data.Maybe (fromJust, isNothing)
import Graphics.Vty
  ( Event (EvKey)
  , Image
  , Key (KChar, KDel, KDown, KEsc, KFun, KLeft, KRight, KUp)
  , Modifier (MCtrl)
  , Output (displayBounds)
  , Vty (nextEvent, outputIface, shutdown, update)
  , brightWhite
  , brightYellow
  , charFill
  , cyan
  , defAttr
  , green
  , imageHeight
  , magenta
  , mkVty
  , pad
  , picForImage
  , red
  , resizeWidth
  , reverseVideo
  , standardIOConfig
  , string
  , translateY
  , withBackColor
  , withForeColor
  , withStyle
  , yellow
  , (<->)
  , (<|>)
  )
import Sound.Sequencer.Editor
  ( Column
      ( EPCol1
      , EPCol16
      , ETCol
      , InsCol1
      , InsCol16
      , NoteCol
      , VolCol1
      , VolCol16
      )
  , Editor (..)
  , EditorEvent
    ( AddRow
    , Edit
    , JumpDown
    , JumpLeft
    , JumpRight
    , JumpUp
    , MoveDown
    , MoveLeft
    , MoveRight
    , MoveUp
    , Quit
    , RemoveRow
    , SelectOctave
    )
  , edit
  )
import Sound.Sequencer.Sequencer
  ( Cell (effectParam, effectType, instrument, note, volume)
  , Sequencer (patterns, playing)
  )

rootImage :: Editor -> Int -> Image
rootImage Editor {..} h =
  renderPatternList Editor {..}
    <|> charFill defAttr ' ' 1 h
    <|> charFill defAttr '|' 1 h
    <|> pad 2 1 1 1 (translateY (if h > imageHeight pat then 0 else scroll - snd cursorY) pat)
 where
  pat =
    foldr1 (<|>) $
      (renderIndex : [renderChannel i | i <- [0 .. numChannels - 1]]) <*> pure Editor {..}
  numChannels = length $ head (snd $ patterns sqncr !! fst cursorY)
  scroll = floor (fromIntegral h / 4)

renderIndex :: Editor -> Image
renderIndex Editor {..} = pad 0 0 2 0 $ foldr1 (<->) [row i | i <- [1 .. length curPat]]
 where
  curPat = snd $ patterns sqncr !! fst cursorY
  row i =
    string
      (defAttr `withForeColor` brightYellow)
      (replicate (4 - length (show i)) '0' ++ show i)

renderChannel :: Int -> Editor -> Image
renderChannel ch Editor {..} = pad 1 0 1 0 $ foldr1 (<->) [renderCell Editor {..} ch i | i <- [0 .. length curPat - 1]]
 where
  curPat = snd $ patterns sqncr !! fst cursorY

renderPatternList :: Editor -> Image
renderPatternList Editor {..} = pad 1 1 1 0 $ resizeWidth 10 $ foldr1 (<->) (f <$> zip (fst <$> patterns sqncr) [0 .. numPat])
 where
  f (n, c) = string ((if c == fst cursorY then flip withStyle reverseVideo else id) defAttr) n
  numPat = length $ patterns sqncr

renderCell :: Editor -> Int -> Int -> Image
renderCell Editor {..} ch i =
  string (w NoteCol brightWhite) (maybe "..." show (note cell))
    <|> string defAttr " "
    <|> string (w InsCol16 cyan) (print16th $ instrument cell)
    <|> string (w InsCol1 cyan) (print1st $ instrument cell)
    <|> string defAttr " "
    <|> string (w VolCol16 green) (print16th $ volume cell)
    <|> string (w VolCol1 green) (print1st $ volume cell)
    <|> string defAttr " "
    <|> string (w ETCol yellow) (print1st $ effectType cell)
    <|> string (w EPCol16 magenta) (print16th $ effectParam cell)
    <|> string (w EPCol1 magenta) (print1st $ effectParam cell)
    <|> string defAttr " "
 where
  cell = snd (patterns sqncr !! fst cursorY) !! i !! ch
  w c t = (if cursorX == (ch, c) && snd cursorY == i then se else id) $ defAttr `withForeColor` t
  se x = if playing sqncr then x `withStyle` reverseVideo else x `withBackColor` red
  print1st a = if isNothing a then "." else [intToDigit . fromIntegral $ fromJust a `mod` 16]
  print16th a = if isNothing a then "." else [intToDigit . fromIntegral $ fromJust a `div` 16]

run :: Editor -> IO ()
run ed = do
  cfg <- standardIOConfig
  vty <- mkVty cfg
  mainLoop vty ed
  shutdown vty

mainLoop :: Vty -> Editor -> IO ()
mainLoop vty Editor {..} = do
  bounds <- displayBounds $ outputIface vty
  update vty $ picForImage $ rootImage Editor {..} (snd bounds)
  when
    running
    (nextEvent vty >>= mainLoop vty . handleEvents Editor {..})

handleEvents :: Editor -> Event -> Editor
handleEvents ed@Editor {..} ev = case ev of
  EvKey KEsc [] -> e Quit
  EvKey (KFun i) [] -> e (SelectOctave i)
  EvKey (KChar '+') [] -> e AddRow
  EvKey (KChar '-') [] -> e RemoveRow
  EvKey (KChar c) [] -> e (Edit c)
  EvKey KDel [] -> e (Edit '\DEL')
  EvKey KUp [] -> e MoveUp
  EvKey KUp [MCtrl] -> e JumpUp
  EvKey KDown [] -> e MoveDown
  EvKey KDown [MCtrl] -> e JumpDown
  EvKey KRight [] -> e MoveRight
  EvKey KRight [MCtrl] -> e JumpRight
  EvKey KLeft [] -> e MoveLeft
  EvKey KLeft [MCtrl] -> e JumpLeft
  _ -> ed
 where
  e = edit ed
