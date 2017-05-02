module HMaze.Random where

import System.Random

randomRs' :: (RandomGen g, Random a) => Int -> (a,a) -> g -> ([a], g)
randomRs' 0 r g = ([], g)
randomRs' n r g = (as : ass, ngg)
  where (as, ng) = randomR r g
        (ass, ngg) = randomRs' (n-1) r ng

randoms' :: (RandomGen g, Random a) => Int -> g -> ([a], g)
randoms' 0 g = ([], g)
randoms' n g = (as : ass, ngg)
  where (as, ng) = random g
        (ass, ngg) = randoms' (n-1) ng


randomER' :: (RandomGen g, Random a, Integral a) => (a,a) -> g -> (a, g)
randomER' r g = (a * 2, g')
  where (a,g') = randomR (fmap (`div` 2) r) g
