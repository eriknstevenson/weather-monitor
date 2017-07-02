{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Logging as Logger
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import DarkSky.Client
import DarkSky.Request
import DarkSky.Response hiding (coordinate)
import DarkSky.Response.DataBlock hiding (icon)
import DarkSky.Response.DataPoint hiding (time)
import DarkSky.Response.Icon
import DarkSky.Types
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector (Vector)
import GHC.Generics
import System.Directory
import qualified Twilio
import qualified Twilio.Messages as Twilio

data Location = Location
  { _locationName :: Text
  , _coordinate :: Coordinate
  } deriving (Show, Eq, Ord, Generic)

instance FromJSON Location where
  parseJSON = withObject "Location" $ \v -> do
    name <- v .: "name"
    lat <- v .: "latitude"
    long <- v .: "longitude"
    return $ Location name (Coordinate lat long)

instance ToJSON Location where
  toJSON (Location name coord) = object
    [ "name" .= name
    , "latitude" .= latitude coord
    , "longitude" .= longitude coord
    ]

data Person = Person 
  { _personName :: Text
  , _phoneNumber :: Text 
  , _subscriptions :: [Text]
  } deriving (Show, Eq, Ord, Generic)

instance FromJSON Person where
  parseJSON = withObject "Person" $ \v -> Person
    <$> v .: "name"
    <*> v .: "phonenumber"
    <*> v .: "subscriptions"

instance ToJSON Person where
  toJSON (Person name phoneNumber subscriptions) = object 
    [ "name" .= name
    , "phonenumber" .= phoneNumber
    , "subscriptions" .= toJSON subscriptions
    ]

data Settings = Settings 
  { _locations :: HM.HashMap Text Coordinate
  , _people :: [Person]} deriving (Show, Eq, Generic)

flattenLocation :: (Text, Coordinate) -> Location
flattenLocation (name, coordinates) = 
  Location name (Coordinate (latitude coordinates) (longitude coordinates))

instance FromJSON Settings where
  parseJSON = withObject "Configuration" $ \v -> do
    (locations :: Vector Object) <- v .: "locations"
    locationsMap <- foldM (\acc loc -> do
      name <- loc .: "name"
      coord <- Coordinate <$> loc .: "latitude" <*> loc .: "longitude"
      return $ HM.insert name coord acc) HM.empty locations
      
    people <- v .: "people"
    return $ Settings locationsMap people

instance ToJSON Settings where
  toJSON (Settings locations people) = object 
      [ "locations" .= toJSON locations'
      , "people" .= toJSON people
      ] 
    where
      locations' :: [Location]
      locations' = map flattenLocation . HM.toList $ locations 

data Configuration = Configuration
  { _twilioSid :: Text
  , _twilioToken :: Text
  , _darkskyKey :: Text
  , _settings :: Settings
  } deriving (Eq, Show)

readSingleLine :: String -> EitherT Text IO Text
readSingleLine filename = do
  fileExist <- liftIO $ doesFileExist filename
  if fileExist 
    then do
      result <- T.strip <$> liftIO (T.readFile filename)
      right result
    else left $ "Unable to find file: " <> T.pack filename

main :: IO ()
main = do
  setCurrentDirectory "/home/erik/weather-monitor/"
  Logger.withStdoutLogging $
    eitherT fatalError fetchAndNotify readConfiguration

optimizeSettings :: Settings -> Settings
optimizeSettings (Settings locations people) = 
    Settings onlySubscribedToLocations people
  where 
    allSubscriptions = concatMap _subscriptions people
    onlySubscribedToLocations = HM.filterWithKey (\location _ -> location `elem` allSubscriptions) locations

getForecastFor :: Text -> Coordinate -> IO Response
getForecastFor darkskyKey coord = 
    getForecast req
  where
    req = Request 
      { key = T.unpack darkskyKey
      , coordinate = coord
      , time = Nothing
      , excludeBlocks = Set.empty
      , extendHourly = False
      , language = Nothing
      , units = Nothing
      }

rainIsForecasted :: Int -> Response -> Bool
rainIsForecasted hours resp = 
    any (\dataPoint -> icon dataPoint `elem` [Just Rain, Just Sleet]) next12Hours
  where
    next12Hours = fromMaybe [] (take (hours - 2) . drop 2 . data' <$> hourly resp)

notifySubscribers :: Text -> Configuration -> IO ()
notifySubscribers location (Configuration twilioSid twilioToken _ settings) =
    forM_ subscribers $ \person -> do
      Logger.log $ "informing " <> _personName person <> " about " <> location
      let message = Twilio.PostMessage
            { Twilio.sendTo = _phoneNumber person
            , Twilio.sendFrom = "+18722105113"
            , Twilio.sendBody = "Lookin' like rain today in " <> location <> ". Bring an umbrella!"}
      void $ Twilio.runTwilio' (return . T.unpack $ twilioSid) (return . T.unpack $ twilioToken) $
        Twilio.post message
      return ()
  where
    subscribers = filter (\person -> location `elem` _subscriptions person) (_people settings)

fetchAndNotify :: Configuration -> IO ()
fetchAndNotify config@(Configuration _ _ darkskyKey settings) = do
  result <- runEitherT $
    forM (HM.keys $ _locations settings) $ \location -> do
      liftIO $ Logger.log $ "Requesting forecast for " <> location
      let mcoord = HM.lookup location (_locations settings)
      case mcoord of
        Nothing -> 
          liftIO $ Logger.log $ "No coordinate information found for " <> location
        Just coord -> do
          resp <- liftIO $ getForecastFor darkskyKey coord
          when (rainIsForecasted 12 resp) (liftIO $ notifySubscribers location config)
          return ()
  case result of
    Left err -> Logger.errorL err
    Right _ -> Logger.log "Complete."
  

fatalError :: Text -> IO ()
fatalError errorMsg = do
  Logger.errorL "Failed to read configuration data."
  Logger.errorL errorMsg

readConfiguration :: EitherT Text IO Configuration
readConfiguration = do
  liftIO (Logger.log "Loading configuration information")
  twilioSid <- readSingleLine "twilio-sid"
  twilioToken <- readSingleLine "twilio-token"
  darkskyKey <- readSingleLine "darksky-key"

  settingsExist <- liftIO $ doesFileExist "settings.json"
  unless settingsExist $
    left "Unable to find settings.json"
  (msettings :: Maybe Settings) <- decode . LBS.pack <$> liftIO (readFile "settings.json")
  case msettings of
    Just settings -> 
      right $ Configuration twilioSid twilioToken darkskyKey (optimizeSettings settings)

    Nothing -> 
      left "Failed to parse settings file"
