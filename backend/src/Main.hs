{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Main where

import Control.Concurrent       (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad.IO.Class   (liftIO)
import Data.Aeson               (ToJSON, FromJSON, decode, (.:), withObject)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types         (parseMaybe)
import Data.ByteString.Lazy     (ByteString)
import Data.Maybe               (mapMaybe)
import Data.Text                (Text)
import qualified Data.Text as T
import Data.Time.Clock          (getCurrentTime, UTCTime, addUTCTime)
import Data.Time.Clock.POSIX    (posixSecondsToUTCTime)
import Data.Time.Format         (formatTime, defaultTimeLocale, iso8601DateFormat)
import GHC.Generics             (Generic)
import Network.HTTP.Simple      (httpLBS, getResponseBody, parseRequest_)
import System.Random            (randomRIO)
import Streamly.Prelude         (SerialT, drain, repeatM)
import qualified Streamly.Prelude as S
import Web.Scotty               (scotty, get, params, json)

-- | Bounding‐box tip (minLat, maxLat, minLon, maxLon)
type BoundingBox = (Double, Double, Double, Double)

-- | Struktura za JSON‐podatak (dummy ili earthquake)
data DataPoint = DataPoint
  { source    :: Text
  , metric    :: Text
  , timestamp :: UTCTime
  , value     :: Double
  , place     :: Text
  } deriving (Show, Generic)

instance ToJSON DataPoint

-- | Dummy generator: stock / crypto
dummyPoint :: IO (Maybe DataPoint)
dummyPoint = do
  threadDelay 1000000  -- 1 sekunda
  now <- getCurrentTime
  i   <- randomRIO (0 :: Int, 1)
  if i == 0
    then do
      v <- randomRIO (100.0, 200.0)
      pure $ Just $ DataPoint "stock"  "AAPL" now v ""
    else do
      v <- randomRIO (20000.0, 40000.0)
      pure $ Just $ DataPoint "crypto" "BTCUSDT" now v ""

--  1. Bounding‐box funkcija za državu 
countryBBox :: Text -> Maybe BoundingBox
countryBBox c = case T.toLower c of
  "croatia" -> Just (42.4, 46.5, 13.5, 19.5)
  "usa"     -> Just (24.5, 49.4, -124.8, -66.9)
  "japan"   -> Just (30.0, 46.0, 129.0, 146.0)
  _         -> Nothing

--  2. Bounding‐box funkcija za regiju unutar države 
regionBBox :: Text -> Text -> Maybe BoundingBox
regionBBox country region = case (T.toLower country, T.toLower region) of
  -- Hrvatska, županije
  ("croatia", "zagreb")               -> Just (45.6, 45.9, 15.8, 16.1)    -- Grad Zagreb
  ("croatia", "splitsko-dalmatinska") -> Just (43.1, 43.7, 16.5, 17.5)    -- Splitsko‐dalmatinska
  ("croatia", "zagrebačka")           -> Just (45.5, 46.0, 15.5, 16.5)    -- Zagrebačka
  -- … dodajte ostale županije

  -- SAD, savezne države
  ("usa", "colorado")   -> Just (36.9924, 41.0024, -109.0603, -102.0420)
  ("usa", "california") -> Just (32.5343, 42.0095, -124.4096, -114.1315)
  ("usa", "texas")      -> Just (25.8371, 36.5007, -106.6456, -93.5083)
  -- … dodajte ostale savezne države

  -- Japan, prefekture
  ("japan", "tokyo")    -> Just (35.5175, 35.8989, 139.2, 139.9)
  ("japan", "osaka")    -> Just (34.4172, 34.8160, 135.3, 135.7)
  -- … dodajte ostale prefekture

  _ -> Nothing

--  3. Struktura za parsiranje GeoJSON‐feature 
data EqFeature = EqFeature
  { magProp   :: Maybe Double
  , timeProp  :: Maybe Integer   -- vrijeme u ms od 1970.
  , placeProp :: Maybe Text      -- npr. "10km NW of Zagreb, Croatia"
  } deriving (Show)

instance FromJSON EqFeature where
  parseJSON = withObject "feature" $ \o -> do
    props <- o .: "properties"
    m     <- props .: "mag"
    t     <- props .: "time"
    p     <- props .: "place"
    pure $ EqFeature m t p

--  4. Oblikovanje bounding‐box query parametara 
formatBBoxParams :: BoundingBox -> String
formatBBoxParams (minLat, maxLat, minLon, maxLon) =
  "&minlatitude="  ++ show minLat
  ++ "&maxlatitude=" ++ show maxLat
  ++ "&minlongitude=" ++ show minLon
  ++ "&maxlongitude=" ++ show maxLon

--  5. Dohvat potresa iz USGS‐a, vraća listu DataPoint ‰
fetchEarthquakeData :: Maybe BoundingBox -> IO (Either String [DataPoint])
fetchEarthquakeData mbbox = do
  now <- getCurrentTime
  let oneDayAgo  = addUTCTime (negate $ 24 * 3600) now
      iso t      = formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) t ++ "Z"
      startParam = "?format=geojson&starttime=" ++ iso oneDayAgo
      bboxParam  = maybe "" formatBBoxParams mbbox
      url        = "https://earthquake.usgs.gov/fdsnws/event/1/query" ++ startParam ++ bboxParam
      request    = parseRequest_ url

  response <- httpLBS request
  let body :: ByteString
      body = getResponseBody response

  case Aeson.decode body of
    Nothing  -> pure $ Left "Neuspjelo dekodiranje GeoJSON"
    Just val -> do
      -- Parsiraj “features” polje iz GeoJSON‐a
      let feats = case parseMaybe (\o -> o .: "features") val of
                    Just arr -> arr
                    Nothing  -> []
      -- Mapiraj svaki element na EqFeature
      let eqs = mapMaybe (parseMaybe Aeson.parseJSON) feats :: [EqFeature]
      -- Pretvori EqFeature → DataPoint (uključujući place)
      let dps = flip map eqs $ \(EqFeature m t p) ->
                 let ts     = posixSecondsToUTCTime $ (/1000) $ fromIntegral (maybe 0 id t)
                     magVal = maybe 0 id m
                     plc    = maybe "" id p
                 in DataPoint "earthquake" "" ts magVal plc
      pure $ Right dps

main :: IO ()
main = do
  -- MVar za dummy tok
  mvar <- newMVar [] :: IO (MVar [DataPoint])

  -- Seed dummy točaka
  mdp0 <- dummyPoint
  case mdp0 of
    Just dp0 -> modifyMVar_ mvar (pure . take 100 . (dp0 :))
    Nothing  -> pure ()

  -- Streamly tok koji svakih ~1s generira dummyPoint
  let stream :: SerialT IO (Maybe DataPoint)
      stream = S.repeatM dummyPoint

      action :: Maybe DataPoint -> IO ()
      action (Just dp) = modifyMVar_ mvar (pure . take 100 . (dp :))
      action Nothing   = pure ()

  _ <- forkIO $ drain $ S.mapM action stream

  -- Scotty HTTP server na portu 3000
  scotty 3000 $ do
    get "/data" $ do
      ps      <- params
      let src      = maybe "all" id $ lookup "source" ps
          country  = maybe "all" id $ lookup "country" ps
          regionMp = lookup "region" ps

      if src == "earthquake"
        then do
          -- Ako je parametar region prisutan, koristi ga prije countryBBox
          let mbbox = case regionMp of
                        Just reg -> regionBBox (T.toLower country) (T.toLower reg)
                        Nothing  -> countryBBox (T.toLower country)
          eqres <- liftIO $ fetchEarthquakeData mbbox
          case eqres of
            Left _    -> json ([] :: [DataPoint])
            Right dps -> json dps
        else do
          -- Dummy tok (stock/crypto)
          let srcMetric = src
          allData <- liftIO $ readMVar mvar
          let filtered = if srcMetric == "all"
                           then allData
                           else filter ((== srcMetric) . metric) allData
          json filtered
