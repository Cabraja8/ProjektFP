{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Main where

import Control.Concurrent       (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad.IO.Class   (liftIO)
import Data.Aeson               (ToJSON)
import Data.Text                (Text)
import Data.Time.Clock          (getCurrentTime, UTCTime)
import GHC.Generics             (Generic)
import System.Random            (randomRIO)
import Streamly.Prelude         (SerialT, drain, repeatM)
import qualified Streamly.Prelude as S
import Web.Scotty               (scotty, get, params, json)

-- | Jedan podatak na vremenskoj liniji
data DataPoint = DataPoint
  { source    :: Text
  , metric    :: Text
  , timestamp :: UTCTime
  , value     :: Double
  } deriving (Show, Generic)

instance ToJSON DataPoint

-- | Generiraj jedan nasumičan dummy DataPoint: naizmjenično AAPL ili BTCUSDT
dummyPoint :: IO (Maybe DataPoint)
dummyPoint = do
  threadDelay 1000000  -- 1 sekunda
  now <- getCurrentTime
  i   <- randomRIO (0::Int,1)
  if i == 0
    then do v <- randomRIO (100.0,200.0)
            pure $ Just $ DataPoint "stock"  "AAPL"    now v
    else do v <- randomRIO (20000.0,40000.0)
            pure $ Just $ DataPoint "crypto" "BTCUSDT" now v

main :: IO ()
main = do
  mvar <- newMVar [] :: IO (MVar [DataPoint])

  -- seed–aj odmah jedan dummy point tako da /data ne vrati prazno
  mdp0 <- dummyPoint
  case mdp0 of
    Just dp0 -> modifyMVar_ mvar (pure . take 100 . (dp0:))
    Nothing  -> pure ()

  -- streamly tok koji svakih ~1s generira dummyPoint
  let stream :: SerialT IO (Maybe DataPoint)
      stream = S.repeatM dummyPoint

      action :: Maybe DataPoint -> IO ()
      action (Just dp) = modifyMVar_ mvar (pure . take 100 . (dp:))
      action Nothing   = pure ()

  -- pokreni streaming u pozadini
  _ <- forkIO $ drain $ S.mapM action stream

  -- Scotty HTTP API
  scotty 3000 $ do
    get "/data" $ do
      ps <- params                     -- sve query-parametre
      let src = maybe "all" id $ lookup "source" ps
      allData <- liftIO $ readMVar mvar
      let filtered = if src == "all"
                       then allData
                       else filter ((== src) . metric) allData
      json filtered