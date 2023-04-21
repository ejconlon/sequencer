module Main
  ( main
  )
where

import Control.Monad ((<=<))
import Sound.Sequencer.Editor
  ( Column (NoteCol)
  , Editor (Editor)
  , loadXM
  )
import Sound.Sequencer.Sequencer (defaultSeq)
import Sound.Sequencer.Vty (run)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= parseArgs

parseArgs :: [String] -> IO ()
parseArgs [] = run $ Editor (0, NoteCol) (0, 0) 4 True defaultSeq
parseArgs fn = mapM_ (maybe (return ()) run <=< loadXM) fn
