{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module HMaze(
  Grid(..),
  Tile(..),
  createMaze
) where
import HMaze.Random
import Control.Monad (forM_, replicateM_)
import Control.Monad.State (State, when, execState, gets, modify, liftIO)
import Data.List (sort)
import Data.Lens.Lazy ((^%=), (%=), (%%=))
import Data.Lens.Template (nameMakeLens)
import Data.Array (Array, listArray, elems, bounds, indices, (//), (!))
import System.Random (newStdGen, StdGen)
import System.Environment (getArgs)

data Tile = Void | Corridor | Room deriving Eq

data Dir = East | West | North | South deriving (Ord, Eq, Show)

type Grid = Array (Int, Int) Tile

data States = States {
  grid :: Grid,
  gen :: StdGen
}

type StateM = State States

$( nameMakeLens ''States (Just . ('l':)) )

getdirs :: StateM [Dir]
getdirs = do
  order <- lgen %%= \g -> randoms' 4 g :: ([Int], StdGen)
  return $ (map snd . sort) $ order `zip` [North, South, East, West]

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

carveMaze s_pos = carve [s_pos] >> step s_pos

createMaze :: Int -> Int -> (Int, Int) -> IO Grid
createMaze width height s_pos = do
  g <- newStdGen
  return $ grid . execState (carveMaze s_pos) $ States initial g
  where
    initial = listArray ((1,1),(height,width)) [Void | _ <- [1..]]

