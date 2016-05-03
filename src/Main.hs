module Main where

import           City                (generateWorld)
import           Constants           (population, ranges)
import           Control.Monad       (forM_, void)
import           Control.Monad.State (StateT, get, liftIO, put, runStateT)
import           Helpers             (inRange)
import           Types
import           Update              (updateCity)

main :: IO ()
main = void $ runStateT process (generateWorld population, [])

process :: StateT (City, [Frame]) IO ()
process = do
  forM_ ranges $ \time -> do
    (city, results) <- get
    c' <- updateCity city time
    let res' = getFrame c' time
    put (c', res':results)
  (_, res) <- get
  liftIO $ execRes res

-- Move later to another file.
getFrame :: City -> Double -> Frame
getFrame c time = let
  users = concatMap _currentState $ _pieces c
  lenghts = [(x, y, formula $ gotUsers users x y) | x <- [0..7], y <- [0..7]]
  in Frame {timeOfFrame = time, globalCapacity = fromIntegral $ length users, piecesCapacity = lenghts}
  where
    gotUsers :: [MU] -> Int -> Int -> [MU]
    gotUsers list x y = filter (\x' -> _current x' == (x, y)) list
    formula list = mconcat $ map (helper . muType) list
    helper x
      | time `inRange` (7.0, 9.0) = case () of _
                                                 | x == Working -> Calls (0.5 / 4, 0, 0)
                                                 | x == Residential -> Calls (0, 0.5 / 4, 0)
                                                 | x == HighMobility -> Calls (0, 0, 1.0 / 4)
      | time `inRange` (9.0, 11.00) = case () of _
                                                   | x == Working -> Calls (2.0 / 4, 0, 0)
                                                   | x == Residential -> Calls (0, 1.5 / 4, 0)
                                                   | x == HighMobility -> Calls (0, 0, 2.0 / 4)

      | otherwise = case () of _
                                 | x == Working -> Calls (3.5 / 4, 0, 0)
                                 | x == Residential -> Calls (0, 2.0 / 4, 0)
                                 | x == HighMobility -> Calls (0, 0, 3.5 / 4)

-- Move later to another file.
execRes :: [Frame] -> IO ()
execRes list = mapM_ (\x -> putStrLn "" *> print x) $ reverse list
