module Lib
  ( search,
  )
where

import Transformations.DataParsing (loadParsedData)
import Types.WebPageData (WebPageData (WebPageData))

search = do
  loadedWebPageData <- loadParsedData "./res/data.jl" :: IO [Maybe WebPageData]
  print loadedWebPageData
