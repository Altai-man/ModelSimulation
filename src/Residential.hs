{-# LANGUAGE RecordWildCards #-}
module Residential where

import           Constants           (resiCarRange, resiPublicRange)
import           Control.Lens        ((.~))
import           Control.Monad.State (MonadIO)
import           GenericMovement     (makeMovement)
import           Helpers
import           Types               (MU (..), MUState (..), muType,
                                      timeOfWaiting)

updateRes :: MonadIO m => MU -> Double -> m MU
updateRes u@MU{..} time
  | time == 7.0 = do
      tow' <- getNumber (0, 120)
      return $ timeOfWaiting .~ Just tow' $ u
  | _state == Busy = if tow u <= 0 -- If user finished his business, he returns to home.
                     then return u { _to = Just _home, _state = Moving }
                          -- Here is a cycle with Busy state. User who finished his work will change his state
                          -- to moving, than he will change to busy and so on without changing position.
                     else return $ timeOfWaiting .~ Just (tow u - 15) $ u
  | _state == Moving =
      if fromMaybeInternal _to /= _current then
        let (xc, yc) = _current
            (xt, yt) = fromMaybeInternal _to
        in makeMovement u (xc, xt) (yc, yt)
      else return u { _state = Busy}
  | _state == Waiting = if tow u <= 15
                        then generateResBusiness u
                        else return $ timeOfWaiting .~ Just (tow u - 15) $ u

generateResBusiness :: MonadIO m => MU -> m MU
generateResBusiness u@MU{..} = do
  -- We assume that probabilities of a and b types are equal.
  random <- getNumber (0, 1)
  if random == 0 then do
    let u' = u { _state = Moving, _to = Just _current }
    timeForBusiness <- getNumber (30, 120)
    return $ u' { _timeOfWaiting = Just timeForBusiness }
    else do
    u' <- chooseDirection u
    timeForB <- getNumber (30, 180)
    vehicle <- genVehicle resiCarRange resiPublicRange
    return $ u' { _state = Moving, tType = vehicle, _timeOfWaiting = Just timeForB }
