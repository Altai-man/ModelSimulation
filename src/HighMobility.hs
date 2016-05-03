{-# LANGUAGE RecordWildCards #-}
module HighMobility where

import           Control.Monad.State (MonadIO)
import           GenericMovement     (makeMovement)
import           Helpers
import           Types               (MU (..), MUState (..))

-- HighMobility stuff.
updateHM :: MonadIO m => MU -> Double -> m MU
updateHM u@MU{..} time
  | time == 7.0 = do -- Initialization.
      u' <- chooseDirection u
      produceUpdatedHM u' -- Initial state is Moving for HM MU.
  | _state == Moving = produceUpdatedHM u
  | _state == Busy = do
      u' <- chooseDirection u
      produceUpdatedHM $! u' { _state = Moving }

produceUpdatedHM :: MonadIO m => MU -> m MU
produceUpdatedHM u@MU{..}
  | fromMaybeInternal _to == _current = return $! u { _state = Busy }
  -- Since we simulate with 15-minutes frame, precisely time of pause doesn't matter,
  -- so we just change state and get 15-minutes pause for MU.
  | otherwise = let (xc, yc) = _current
                    (xt, yt) = fromMaybeInternal _to
                in f =<< makeMovement u (xc, xt) (yc, yt)
  where
    f u'@MU{..} = if fromMaybeInternal _to == _current
                  then return $ u' { _state = Busy }
                  else return u'
