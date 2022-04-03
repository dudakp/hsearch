{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( search,
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

loadFile :: FilePath -> IO ByteString
loadFile = Data.ByteString.Lazy.readFile

instance FromJSON WebPageData where
  parseJSON (Object v) = do
    url <- v .: "url"
    htmlContent <- v .: "html_content"
    return (WebPageData {url = url, htmlContent = htmlContent})
  parseJSON _ = empty

search = do
  res <- loadFile "./res/data.jl"
  let byLine = split (c2w '\n') res
  let loadedWebPageData = map (\line -> decode line :: Maybe WebPageData) byLine
  print loadedWebPageData
