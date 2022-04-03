{-# LANGUAGE OverloadedStrings #-}

module Transformations.DataParsing
  ( loadParsedData
  )
where

import Control.Applicative (Alternative (empty))
import Control.Monad ()
import Data.Aeson
  ( FromJSON (parseJSON),
    Value (Object),
    decode,
    encode,
    (.:),
  )
import Data.ByteString.Internal (c2w, w2c)
import Data.ByteString.Lazy (ByteString, readFile, split)
import Data.Functor (fmap)
import GHC.Generics ()
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
  let byLine = split (c2w '\n') res
  let res = map (\line -> decode line :: Maybe WebPageData) byLine :: [Maybe WebPageData]
  return res