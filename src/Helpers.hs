{-# LANGUAGE RecordWildCards #-}
module Helpers where

import           Constants
import           Control.Monad        (liftM)
import           Control.Monad.Random (evalRandIO, getRandomR)
import           Control.Monad.State  (MonadIO, liftIO)
import           Data.Function        (on)
import           Data.List            (sort)
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (fromMaybe)
import           Types

-- Unsafe functions.
fromMaybeInternal :: Maybe a -> a
fromMaybeInternal (Just a) = a
fromMaybeInternal Nothing = error "fromMaybeInternal"

getKey :: Ord k => k -> Map.Map k b -> b
getKey key st' = fromMaybeInternal $! Map.lookup key st'

-- Clean functions.
enumTo :: (Enum a, Num a) => a -> [a]
enumTo = enumFromTo 1

division :: Int -> Int -> Double
division = (/) `on` fromIntegral

getRounded :: (Integral b, RealFrac r) => r -> r -> b
getRounded num per = truncate $! num * per

tow :: MU -> Int
tow u = fromMaybe 0 $ _timeOfWaiting u

inRange :: Ord a => a -> (a, a) -> Bool
inRange num (min', max') = (min' <= num) && (num <= max')

-- Dirty function.
genVehicle :: MonadIO m => (Int, Int) -> (Int, Int) -> m (Maybe Vehicle)
genVehicle carRange publicRange = liftM dispatcher $ getNumber (1, 100)
  where
    dispatcher rand
      | rand `inRange` carRange = Just Car
      | rand `inRange` publicRange = Just PublicTransport
      | otherwise = error "genVehicle"

getNumber :: MonadIO m => (Int, Int) -> m Int
getNumber = liftIO . evalRandIO . getRandomR

x :: MonadIO m => m Int
x = getNumber (0, 7)

chooseDirection :: MonadIO m => MU -> m MU
chooseDirection u@MU{..} = do
  rand <- getNumber (1, 100)
  direct <- chooseCoords rand muType
  return $! u { _to = Just direct }

chooseCoords :: MonadIO m => Int -> MUCat -> m (Int, Int)
chooseCoords rand Working
  -- FIXME.
  | rand `inRange` workingCenterRange = do
      y <- getNumber centerRange
      x >>= (\x' -> return (x', y))
  | rand `inRange` workingUrbanRange = do
      y <- getNumber urbanRange
      x >>= (\x' -> return (x', y))
  | rand `inRange` workingSubRange = do
      y <- getNumber subUrbanRange
      x >>= (\x' -> return (x', y))
  | rand `inRange` workingRuralRange = do
      y <- getNumber ruralRange
      x >>= (\x' -> return (x', y))
chooseCoords rand HighMobility
  | rand `inRange` hmCenterRange = do
      y <- getNumber centerRange
      x >>= (\x' -> return (x', y))
  | rand `inRange` hmUrbanRange = do
      y <- getNumber urbanRange
      x >>= (\x' -> return (x', y))
  | rand `inRange` hmSubRange = do
      y <- getNumber subUrbanRange
      x >>= (\x' -> return (x', y))
  | rand `inRange` hmRuralRange = do
      y <- getNumber ruralRange
      x >>= (\x' -> return (x', y))
chooseCoords rand Residential
  | rand `inRange` resCenterRange = do
      y <- getNumber centerRange
      x >>= (\x' -> return (x', y))
  | rand `inRange` resUrbanRange = do
      y <- getNumber urbanRange
      x >>= (\x' -> return (x', y))
  | rand `inRange` resSubRange = do
      y <- getNumber subUrbanRange
      x >>= (\x' -> return (x', y))
  | rand `inRange` resRuralRange = do
      y <- getNumber ruralRange
      x >>= (\x' -> return (x', y))
chooseCoords _ _ = error "chooseCoords"

-- Probably deprecated.
getCityPart :: City -> AreaType -> [Piece]
getCityPart city t = filter (\x -> t == _pAreaType x) $! _pieces city

inversedGetCityPart :: City -> AreaType -> [Piece]
inversedGetCityPart city t = filter (\x -> t /= _pAreaType x) $! _pieces city

includeCityPart :: City -> [Piece] -> City
includeCityPart city injection = City $! circle'
  where
    segs' = inversedGetCityPart city (_pAreaType $! head injection)
    included = segs' ++ injection
    center' = filter (\x -> Center /= _pAreaType x) included
    urban' = filter (\x -> Urban /= _pAreaType x) included
    sub' = filter (\x -> Suburban /= _pAreaType x) included
    rural' = filter (\x -> Rural /= _pAreaType x) included
    circle' = concat [choosing' x | x <- list]
    choosing' x
      | x == 0 = h center'
      | x == 1 = l center'
      | x == 2 = h urban'
      | x == 3 = l urban'
      | x == 4 = h sub'
      | x == 5 = l sub'
      | x == 6 = h rural'
      | x == 7 = l rural'
      | otherwise = error "Choosing"
    h = take 8 . sort
    l = drop 8 . sort
