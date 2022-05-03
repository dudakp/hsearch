{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
module Lib
  ( search,
  )
where

import Data.Traversable (for)
import Transformations.DataParsing (loadParsedData)
import Transformations.Extraction (executePipeline)
import Types.Index (ForwardIndex (ForwardIndex))
import Types.WebPageData (WebPageData (WebPageData))
import Types.WebPageDataProcessed (WebPageDataProcessed (WebPageDataProcessed), words)
import qualified Types.WebPageDataProcessed as WebPageDataProcessed

search = do
  loadedWebPageData <- loadParsedData "./res/datalarge2.jl" :: IO [Maybe WebPageData]
  let res = map executePipeline loadedWebPageData :: [Maybe ForwardIndex]
  print res

check :: Maybe WebPageDataProcessed -> [String]
check (Just d) = Types.WebPageDataProcessed.words d
check Nothing = undefined
