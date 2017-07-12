{-# LANGUAGE OverloadedStrings #-}

module BigchainDB.Client where


import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple


getIndex :: IO ()
getIndex = do
  response <- httpLBS "http://localhost:9984/"
  putStrLn $ "The status code was: " ++
             show (getResponseStatusCode response)
  print $ getResponseHeader "Content-Type" response
  L8.putStrLn $ getResponseBody response
