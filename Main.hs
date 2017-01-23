{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module Main where

import Control.Monad (forM_)
import Control.Monad.State (State, when, execState, gets, modify, liftIO)
import Data.List (sort)
import Data.Lens.Lazy ((^%=), (%=), (%%=))
import Data.Lens.Template (nameMakeLens)
import Data.Text (unpack, pack, chunksOf, intercalate)
import Data.Array (Array, listArray, elems, bounds, indices, (//), (!))
import System.Random (Random, newStdGen, randoms)
import System.Environment (getArgs)

data Tile = Void | Corridor | Room deriving Eq

data Dir = East | West | North | South deriving (Ord, Eq, Show)

instance Show Tile where
  show Void =     "█"
  show Corridor = " "
  show Room =     " "

type Grid = Array (Int, Int) Tile

data States = States {
  grid :: Grid,
  rands :: [Int]
}

type StateM = State States

$( nameMakeLens ''States (Just . ('l':)) )

showGrid :: Grid -> String
showGrid g = unpack it
  where fs = concatMap show (elems g)
        ft = pack fs
        st = chunksOf w ft
        it = intercalate (pack "\n") st
        (_, (_,w)) = bounds g

getdirs :: StateM [Dir]
getdirs = do
  dirs <- gets $ take 4 . rands
  lrands %= drop 4
  return $ snd . unzip . sort $ dirs `zip` [North, South, East, West]

translate :: (Int,Int) -> Dir -> [(Int,Int)]
translate (x,y) West = [(x-1, y), (x-2, y)]
translate (x,y) East = [(x+1, y), (x+2, y)]
translate (x,y) North = [(x, y+1), (x, y+2)]
translate (x,y) South = [(x, y-1), (x, y-2)]

isVoid :: (Int,Int) -> StateM Bool
isVoid (x,y) = do
  g <- gets grid
  return $ (g ! (x,y)) == Void

valid :: [(Int,Int)] -> StateM Bool
valid steps = do
  let (x,y) = last steps
  bounds <- gets $ indices . grid
  if (x,y) `elem` bounds then isVoid (x,y) else return False

carve :: [(Int,Int)] -> StateM ()
carve cells = lgrid %%= \x->((), x // [(c, Corridor) | c <- cells])

step :: (Int,Int) -> StateM ()
step current = do 
  cells <- fmap (fmap (translate current)) getdirs
  forM_ cells $ \c -> valid c >>= flip when (carve c >> step (last c))

(width, height, startx, starty) = (65, 11, 2,2)
initial = listArray ((1,1),(height,width)) [Void | _ <- [1..]]

main :: IO ()
main = newStdGen >>= putStrLn . showGrid . grid . execState (carve [(startx, starty)] >> step (startx,starty)) . States initial . randoms

