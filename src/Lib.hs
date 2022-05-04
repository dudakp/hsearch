{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
module Lib
  ( search,
  )
where

import Data.Traversable (for)
import Transformations.DataParsing (loadParsedData)
import Transformations.Extraction (executePipeline)
import Types.Index (ForwardIndex (ForwardIndex, document, words))
import Types.WebPageData (WebPageData (WebPageData))
import Types.WebPageDataProcessed (WebPageDataProcessed (WebPageDataProcessed), words)
import qualified Types.WebPageDataProcessed as WebPageDataProcessed

search = do
  loadedWebPageData <- loadParsedData "./res/collection_reduced.jl" :: IO [Maybe WebPageData]
  let res = map executePipeline loadedWebPageData :: [Maybe ForwardIndex]
  let urls = fmap (\o -> Types.Index.document o) <$> res
  print urls

check :: Maybe WebPageDataProcessed -> [String]
check (Just d) = Types.WebPageDataProcessed.words d
check Nothing = undefined
