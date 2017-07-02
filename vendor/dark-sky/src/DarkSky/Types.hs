{-# LANGUAGE DeriveGeneric #-}
module DarkSky.Types where

import Data.Aeson
import Data.Scientific (Scientific)
import GHC.Generics

data Coordinate = Coordinate
  { latitude :: Degrees
  , longitude :: Degrees
  } deriving (Eq, Show, Ord, Generic)

instance FromJSON Coordinate
instance ToJSON Coordinate

type Degrees = Scientific

type Temperature = Scientific

type Distance = Scientific

type PrecipitationAmount = Scientific
