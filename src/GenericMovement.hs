{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
module GenericMovement where

import           Constants
import           Control.Lens        ((^.))
import           Control.Monad.State (MonadIO, MonadState, evalState, get, put)
import           Types               hiding (to)
import Helpers


makeMovement :: MonadIO m => MU -> (Int, Int) -> (Int, Int) -> m MU
makeMovement u@MU{..} (xc, xt) (yc, yt)
  -- If user already in car or public transport, we calculate his movement for 15 minutes.
  | yc > yt = do -- If user moves to center.
      let (y'', _) = calculateWayY (fromMaybeInternal tType) yc yt 15.0
      return $ u { _current = (xc, y'')}
  | yc < yt = -- If user moves to outer zone.
    case () of _
                 | xc /= xt -> do -- First we move it peripheral...
                     let (x'', distance') =  calculateWayX (fromMaybeInternal tType) xc xt yc (u^.distance)
                     return $ u { _current = (x'', yc), _distance = distance' }
                 | xc == xt -> do -- Then we move it radial.
                     let (y'', distance') =  calculateWayY (fromMaybeInternal tType) yc yt 15.0
                     return $ u { _current = (xt, y''), _distance = distance' }
                 | otherwise -> error "makeMovement"
  | yc == yt = do -- Movement is only radial.
      let (x'', distance') =  calculateWayX (fromMaybeInternal tType) xc xt yt (u^.distance)
      return $ u { _current = (x'', yt), _distance = distance' }
  | otherwise = error "makeMovementOtherwise"

calculateWayX :: Vehicle -> Int -> Int -> Int -> Double -> (Int, Double)
calculateWayX transport from to circle dist' = evalState (calculatorX transport circle) (from, to, dist')

-- Imperative way with MonadState.
calculatorX :: MonadState (Int, Int, Double) m => Vehicle -> Int -> m (Int, Double)
calculatorX tran circle = do
  (from, to, dist') <- get
  let areatype = getKey circle circleNumToZone
  let pieceWidth = getKey areatype widthSizes / 8 -- Size of one piece.
  let drove = getKey (areatype, tran) speeds / 4 -- Distance for 15 minutes.
  let piecesDrove = (drove / pieceWidth) + dist'
  let fracPart = snd (properFraction piecesDrove :: (Int, Double))
  let step = if from < to then 1 else (-1)
  if (truncate piecesDrove :: Integer) >= 1 then
    if from + truncate piecesDrove >= to then
      put (to, to, 0) else
      put (from + (step * truncate piecesDrove), to, fracPart)
    else put (from, to, fracPart)
  (from', _, dist'') <- get
  return (from', dist'')

calculateWayY :: Vehicle -> Int -> Int -> Double -> (Int, Double)
calculateWayY transport fromY toY time = evalState (calculatorY transport) (fromY, toY, time)

-- Imperative way with MonadState.
-- Still needs improvement.
calculatorY :: MonadState (Int, Int, Double) m => Vehicle -> m (Int, Double)
calculatorY tran = do
  (from, to, time) <- get
  func from to time -- We cut some distance here, actually. FIXME if really needed.
  (from', _, time') <- get
  return (from', time')
 where
   func from to time = do
     let areatype = getKey from circleNumToZone
     let drove = getKey (areatype, tran) speeds / 4 -- How much will drive for 15 minutes.
     let consumedTime = drove / pieceHeight
     let step = if from < to then 1 else (-1)
     if time - consumedTime > 1 then
       if from + (1 * step) == to then put (to, to, 0)
        else put (from + (1 * step), to, time - consumedTime)
       else put (from, to, time - consumedTime)
