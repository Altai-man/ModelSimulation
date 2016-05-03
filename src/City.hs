module City where

import           Constants
import           Helpers
import           Types

generateWorld :: Int -> City
generateWorld pop = initialCity
  where
    initialCity = City $! concat [generateCircle y pop | y <- list]

generateCircle :: Int -> Int -> [Piece]
generateCircle y total
  | y == 0 || y == 1 = generateCentral y total
  | y == 2 || y == 3 = generateUrban y total
  | y == 4 || y == 5 = generateSubUrban y total
  | y == 6 || y == 7 = generateRural y total
generateCircle _ _ = error "Wrong using of direction function."

generateState :: (Int, Int) -> Int -> [MU]
generateState curr total = let
  working = [MU Working Nothing Nothing curr curr Nothing 0.0 Waiting |
             _ <- enumTo (formula workingProb)]
  residen = [MU Residential Nothing Nothing curr curr Nothing 0.0 Waiting |
             _ <- enumTo (formula residentProb)]
  highmob = [MU HighMobility Nothing Nothing curr curr (Just Car) 0.0 Moving |
             _ <- enumTo (formula highmobProb)]
  in working ++ residen ++ highmob
  where
    formula :: Double -> Int
    formula num = getRounded num (division total 100)

-- GENERATORS.
generateCentral :: Int -> Int -> [Piece]
generateCentral y num =
  [Piece Center
   (x, y) (generateState (x, y) (f (5 `division` 8))) | x <- list]
  where
    f = getRounded (num `division` 100)

generateUrban :: Int -> Int -> [Piece]
generateUrban y num =
  [Piece Urban
   (x, y) (generateState (x, y) (f (15 `division` 8))) | x <- list]
  where
    f = getRounded (num `division` 100)

generateSubUrban :: Int -> Int -> [Piece]
generateSubUrban y num =
  [Piece Suburban
   (x, y) (generateState (x, y) (f (25 `division` 8))) | x <- list]
  where
    f = getRounded (num `division` 100)

generateRural :: Int -> Int -> [Piece]
generateRural y num =
  [Piece Rural
   (x, y) (generateState (x, y) (f (5 `division` 8))) | x <- list]
  where
    f = getRounded (num `division` 100)
