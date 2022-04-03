module Lib
  ( search,
  )
where

import Transformations.DataParsing (loadParsedData)
import Transformations.Extraction (processData)
import Types.WebPageData (WebPageData (WebPageData))

search = do
  loadedWebPageData <- loadParsedData "./res/data.jl" :: IO [Maybe WebPageData]
  let res = map processData loadedWebPageData
  print res
