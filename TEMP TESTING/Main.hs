-- src/Main.hs
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Network.HTTP.Simple
import Data.Text.Lazy            (Text)
import Data.Text.Lazy.Encoding  (encodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as BL

main :: IO ()
main = do
  putStrLn "Backend proxy listening on http://localhost:3000"
  scotty 3001 $ do
    -- Proxy za v1 searchteams.php?t={team}
    get "/v1/searchteams" $ do
      team <- param "t"                            -- dobijes query param t
      let baseUrl = "https://www.thesportsdb.com/api/v1/json/123/searchteams.php"
      req0 <- liftIO $ parseRequest baseUrl        -- osnovni GET request
      -- dodajemo query string ?t={team}
      let req1 = setRequestQueryString
                    [("t", Just (BL.toStrict $ encodeUtf8 team))]
                    req0
      response <- liftIO $ httpLBS req1            -- izvrsavamo HTTP GET
      setHeader "Content-Type" "application/json"
      raw (getResponseBody response)               -- vratimo sirovu JSON body
