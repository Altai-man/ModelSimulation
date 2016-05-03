{-# LANGUAGE FlexibleContexts      #-}
module Update where
import           Control.Lens         (mapMOf)
import           Control.Monad.State  (MonadIO)
import           HighMobility         (updateHM)
import           Residential          (updateRes)
import           Types                (City (..), MU (..), MUCat (..),
                                       Piece (..), currentState, muType, pieces)
import           Working              (updateWorking)

updateCity :: MonadIO m => City -> Double -> m City
updateCity c time = mapMOf (pieces . traverse) (`genericUpdate` time) c

genericUpdate :: MonadIO m => Piece -> Double -> m Piece
genericUpdate p time = mapMOf (currentState . traverse) (`updateUser` time) p

updateUser :: MonadIO m => MU -> Double -> m MU
updateUser u time
  | muType u == Working = updateWorking u time
  | muType u == Residential = updateRes u time
  | muType u == HighMobility = updateHM u time
updateUser _ _ = error "updateUser"
