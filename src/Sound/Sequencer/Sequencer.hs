module Sound.Sequencer.Sequencer (Channel (..), Sequencer (..), defaultSeq, emptyCell, module Codec.Tracker.XM.Pattern, module Codec.Tracker.Common) where

import Codec.Tracker.Common
import Codec.Tracker.XM.Pattern (Cell (..), Note (..))
import Data.Word

data Channel = Channel
  { channelVolume :: Word8
  , last :: Maybe (Note, Word8)
  }

data Sequencer = Sequencer
  { channels :: [Channel]
  , patterns :: [(String, [[Cell]])]
  , order :: [Int]
  , pos :: (Int, Int)
  , playing :: Bool
  }

defaultSeq = Sequencer [Channel 100 Nothing, Channel 100 Nothing] [("test", defaultPattern), ("bla", defaultPattern), ("blup", defaultPattern)] [0] (0, 0) False

emptyCell = Cell Nothing Nothing Nothing Nothing Nothing

defaultPattern :: [[Cell]]
defaultPattern =
  [
    [ Cell (Just (Note (Pitch C 3))) (Just 0) Nothing Nothing Nothing
    , Cell (Just (Note (Pitch C 4))) (Just 1) Nothing Nothing Nothing
    ]
  , [emptyCell, emptyCell]
  , [emptyCell, emptyCell]
  , [emptyCell, emptyCell]
  ,
    [ emptyCell
    , Cell (Just (Note (Pitch D 4))) (Just 1) Nothing Nothing Nothing
    ]
  ,
    [ Cell (Just (Note (Pitch C 3))) (Just 0) Nothing Nothing Nothing
    , Cell (Just (Note (Pitch C 4))) (Just 1) Nothing Nothing Nothing
    ]
  ,
    [ Cell (Just (Note (Pitch Asharp 2))) (Just 0) Nothing Nothing Nothing
    , Cell (Just (Note (Pitch Asharp 3))) (Just 1) Nothing Nothing Nothing
    ]
  , [emptyCell, emptyCell]
  , [emptyCell, emptyCell]
  , [emptyCell, emptyCell]
  , [emptyCell, emptyCell]
  ,
    [ emptyCell
    , Cell (Just (Note (Pitch Asharp 3))) (Just 1) Nothing Nothing Nothing
    ]
  ,
    [ Cell (Just (Note (Pitch Gsharp 2))) (Just 0) Nothing Nothing Nothing
    , Cell (Just (Note (Pitch E 3))) (Just 1) Nothing Nothing Nothing
    ]
  ,
    [ emptyCell
    , Cell (Just (Note (Pitch Gsharp 3))) (Just 1) Nothing Nothing Nothing
    ]
  , [emptyCell, emptyCell]
  , [emptyCell, emptyCell]
  , [emptyCell, emptyCell]
  , [emptyCell, emptyCell]
  ,
    [ Cell (Just (Note (Pitch G 2))) (Just 0) Nothing Nothing Nothing
    , Cell (Just (Note (Pitch G 3))) (Just 1) Nothing Nothing Nothing
    ]
  , [emptyCell, emptyCell]
  , [emptyCell, emptyCell]
  ,
    [ Cell (Just (Note (Pitch Asharp 2))) (Just 0) Nothing Nothing Nothing
    , Cell (Just (Note (Pitch F 4))) (Just 1) Nothing Nothing Nothing
    ]
  ,
    [ Cell (Just (Note (Pitch G 2))) (Just 0) Nothing Nothing Nothing
    , Cell (Just (Note (Pitch Dsharp 4))) (Just 1) Nothing Nothing Nothing
    ]
  ,
    [ Cell (Just (Note (Pitch Asharp 2))) (Just 0) Nothing Nothing Nothing
    , Cell (Just (Note (Pitch D 4))) (Just 1) Nothing Nothing Nothing
    ]
  ]
