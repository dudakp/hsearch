{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module Lib
  ( search,
  )
where

import Transformations.DataParsing (loadParsedData)
import Transformations.Extraction (executePipeline)
import Types.WebPageData (WebPageData (WebPageData))
import Data.Traversable (for)
import Types.WebPageDataProcessed (WebPageDataProcessed(WebPageDataProcessed), words)
import qualified Types.WebPageDataProcessed as WebPageDataProcessed

search = do
  loadedWebPageData <- loadParsedData "./res/data.jl" :: IO [Maybe WebPageData]
  let res = map executePipeline loadedWebPageData :: [Maybe WebPageDataProcessed]
  -- let l = concatMap check res
  -- mapM_ print l
  print res

check :: Maybe WebPageDataProcessed -> [String]
check (Just d) = Types.WebPageDataProcessed.words d
check Nothing = undefined


