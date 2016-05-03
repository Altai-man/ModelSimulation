{-# LANGUAGE FlexibleInstances #-}
module Constants where

import qualified Data.Map.Strict as Map
import           Types

-- Variables:
population :: Int
population = 100000

peopleInPiece :: Double
peopleInPiece = fromIntegral population / 64.0

-- Speed range.
speeds :: Map.Map Speed Double
speeds = Map.fromList [((Center, PublicTransport), 7.5),
                       ((Center, Car), 15.0),
                       ((Urban, PublicTransport), 20.0),
                       ((Urban, Car), 27.5),
                       ((Suburban, PublicTransport), 50.0),
                       ((Suburban, Car), 60.0),
                       ((Rural, PublicTransport), 70.0),
                       ((Rural, Car), 80.0)]

{- City sizes -}
pieceHeight :: Double
pieceHeight = 2.5

widthSizes :: Map.Map AreaType Double
widthSizes = Map.fromList [(Center, 7.85), -- (5pi + 10pi) / 2
                           (Urban, 54.97), -- (15pi + 20pi) / 2
                           (Suburban, 86.39), -- (25pi + 30pi) / 2
                           (Rural, 117.8)] -- (35pi + 40pi) / 2

-- Probability of user types.
workingProb :: Double
workingProb = 60

residentProb :: Double
residentProb = 15

highmobProb :: Double
highmobProb = 25

-- Probability of transport types.
workCarRange :: (Int, Int)
workCarRange = (1, 40)

workPublicRange :: (Int, Int)
workPublicRange = (41, 100)

resiCarRange :: (Int, Int)
resiCarRange = (1, 34)

resiPublicRange :: (Int, Int)
resiPublicRange = (35, 100)

-- Probability of work for working people.
workingCenterRange :: (Int, Int)
workingCenterRange = (1, 35)

workingUrbanRange :: (Int, Int)
workingUrbanRange = (36, 70)

workingSubRange :: (Int, Int)
workingSubRange = (71, 95)

workingRuralRange :: (Int, Int)
workingRuralRange = (96, 100)

-- Probability of MAP for HighMobility users.
hmCenterRange :: (Int, Int)
hmCenterRange = (1, 25)

hmUrbanRange :: (Int, Int)
hmUrbanRange = (26, 50)

hmSubRange :: (Int, Int)
hmSubRange = (51, 75)

hmRuralRange :: (Int, Int)
hmRuralRange = (76, 100)

-- Probability of MAP for Residentials.
resCenterRange :: (Int, Int)
resCenterRange = (1, 40)

resUrbanRange :: (Int, Int)
resUrbanRange = (41, 75)

resSubRange :: (Int, Int)
resSubRange = (75, 97)

resRuralRange :: (Int, Int)
resRuralRange = (98, 100)

-- Constants:
-- Time range.
ranges :: [Double]
ranges = [7.0, 7.15, 7.30, 7.45,
          8.0, 8.15, 8.30, 8.45,
          9.0, 9.15, 9.30, 9.45,
          10.0, 10.15, 10.30, 10.45,
          11.0, 11.15, 11.30, 11.45, 12.00]

-- Number of circles.
list :: [Int]
list = [0..7]

-- Ranges of city circles.
centerRange :: (Int, Int)
centerRange = (0, 1)

urbanRange :: (Int, Int)
urbanRange = (2, 3)

subUrbanRange :: (Int, Int)
subUrbanRange = (4, 5)

ruralRange :: (Int, Int)
ruralRange = (6, 7)

-- Simple map for easy lookuping of areatype from number of circle.
circleNumToZone :: Map.Map Int AreaType
circleNumToZone = Map.fromList [(0, Center),
                                (1, Center),
                                (2, Urban),
                                (3, Urban),
                                (4, Suburban),
                                (5, Suburban),
                                (6, Rural),
                                (7, Rural)]
