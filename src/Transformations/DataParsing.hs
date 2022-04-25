{-# LANGUAGE OverloadedStrings #-}

module Transformations.DataParsing
  ( loadParsedData,
  )
where

import Control.Applicative (Alternative (empty))
import Control.Monad ()
import Data.Aeson
  ( FromJSON (parseJSON),
    Value (Object),
    decode,
    (.:),
  )
import Data.ByteString.Internal (c2w)
import Data.ByteString.Lazy (ByteString, readFile, split)
import Data.Maybe (isJust)
import Types.WebPageData (WebPageData (WebPageData, htmlContent, url))

-- | Load file and return result with format appropriate for eason parsing
loadFile :: FilePath -> IO ByteString
loadFile = Data.ByteString.Lazy.readFile

-- | Define parsing scheme for record WebPageData
instance FromJSON WebPageData where
  parseJSON (Object v) = do
    url <- v .: "url"
    htmlContent <- v .: "html_content"
    return (WebPageData {url = url, htmlContent = htmlContent})
  parseJSON _ = empty

-- | Parse data to WebPageData record format
loadParsedData :: FilePath -> IO [Maybe WebPageData]
loadParsedData file = do
  res <- loadFile file
  let byLine = split (c2w '\n') res :: [ByteString]
  let decoded = map (\line -> decode line :: Maybe WebPageData) byLine
  let r = filter isJust decoded
  return r
