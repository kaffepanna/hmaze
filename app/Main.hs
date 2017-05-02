module Main where

import HMaze
import Data.Array (elems, bounds)
import Data.Text (unpack, pack, chunksOf, intercalate)

instance Show Tile where
  show Void =     "█"
  show Corridor = " "
  show Room =     "."

showGrid g = unpack it
  where fs = concatMap show (elems g)
        ft = pack fs
        st = chunksOf w ft
        it = intercalate (pack "\n") st
        (_, (_,w)) = bounds g

main :: IO ()
main = putStrLn . showGrid =<< createMaze 65 21 (2,2)
