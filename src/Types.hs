{-# LANGUAGE TemplateHaskell #-}
module Types where

import           Control.Lens  (makeLenses, view)
import           Data.Function (on)

data AreaType = Center | Urban | Suburban | Rural
              deriving (Show, Eq, Ord)

data Vehicle = Car | PublicTransport | Pedestrian
             deriving (Show, Eq, Ord)

data MUCat = Working | Residential | HighMobility
           deriving (Show, Eq)

data MUState = Waiting | Moving | Busy
             deriving (Show, Eq)

-- City types.
data City = City {
  _pieces :: ![Piece]
  }

data Piece = Piece {
  _pAreaType      :: !AreaType
  , _coords       :: !(Int, Int)
  , _currentState :: ![MU]
  } deriving (Show)

-- Mobile user types.
data MU = MU {
  muType           :: !MUCat
    -- Index of end of path.
  , _to            :: Maybe (Int, Int)
    -- Number of minutes until user starts his trip, if nothing MU is moving.
  , _timeOfWaiting :: Maybe Int
  , _home          :: !(Int, Int)
    -- Index of place in city
  , _current       :: !(Int, Int)
  , tType          :: Maybe Vehicle
    -- It's a distance that MU passed in one drove in terms of one piece.
  , _distance      :: !Double
  , _state         :: !MUState
  } deriving (Show)

makeLenses ''City
makeLenses ''Piece
makeLenses ''MU

-- Working, Residential, HighMobility.
data PhoneCalls = Calls {-# UNPACK #-} !(Double, Double, Double)
                deriving (Show)

instance Monoid PhoneCalls where
  mempty = Calls (0, 0, 0)
  mappend (Calls (a, b, c)) (Calls (a', b', c')) = Calls (a+a', b+b', c+c')

data Frame = Frame {
  timeOfFrame      :: Double
  , globalCapacity :: Double
  , piecesCapacity :: [(Int, Int, PhoneCalls)] -- More parameters can be here.
  } deriving (Show)

type Speed = (AreaType, Vehicle)

instance Show City where
  show c = concatMap printP parts'
    where
      parts' = _pieces c
      printP :: Piece -> String
      printP p = if null (view currentState p) then "" else "This piece:\n" ++ at' p ++ co p ++ "Citizens is: " ++ cu p ++ "\n"
        where
          at' = (++"\n") . show . _pAreaType
          co = (++"\n") . show . _coords
          cu = (++"\n") . show . _currentState

instance Eq Piece where
  (==) = (==) `on` _coords

instance Ord Piece where
  -- We compare coords of 'a' and 'b' in idiomatic form.
  compare = compare `on` _coords
