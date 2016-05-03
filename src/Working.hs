{-# LANGUAGE RecordWildCards #-}
module Working where

import           Constants
import           Control.Lens         ((.~))
import           Control.Monad.State (MonadIO)
import           Data.Maybe           (isNothing)
import           GenericMovement      (makeMovement)
import           Helpers
import           Types

-- Working stuff.
updateWorking :: MonadIO m => MU -> Double -> m MU
updateWorking u@MU{..} time
  | time == 7.0 = do -- Initialization.
      tow' <- getNumber (0, 30)
      return $! timeOfWaiting .~ Just tow' $ u
  | _state == Busy = return u -- If MU is working we don't change his state.
  | _state == Moving = moveWorking u
  | _state == Waiting =
    if tow u <= 15
    then produceUpdatedW u
         -- If person went out and direction will be choosed.
    else return $ timeOfWaiting .~ Just (tow u - 15) $ u
         -- If person still at home.
updateWorking _ _ = error "updateWorking"

produceUpdatedW :: MonadIO m => MU -> m MU
produceUpdatedW u@MU{..}
  -- When MU already got destination.
  | isNothing _timeOfWaiting = moveWorking u
  | otherwise = do
      let u' = timeOfWaiting .~ Nothing $! u
      u'' <- chooseDirection u'
      return $ u'' { tType = Just Pedestrian }

moveWorking :: MonadIO m => MU -> m MU
moveWorking u@MU{..}
  -- If user is just pedestrian and walking,
  -- we assume he'll just change his type of vehicle after ~15 minutes.
  | tType == Just Pedestrian = do
      vehicle <- genVehicle workCarRange workPublicRange
      return $ u { _state = Moving, tType = vehicle }
  -- If man is at workplace, his state will be changed and he will not be updated anymore in simulation.
  | _current == fromMaybeInternal _to = return $ u { _state = Busy }
  | _current /= fromMaybeInternal _to =
      let (xc, yc) = _current
          (xt, yt) = fromMaybeInternal _to
      in makeMovement u (xc, xt) (yc, yt)
moveWorking _ = error "moveWorking"
