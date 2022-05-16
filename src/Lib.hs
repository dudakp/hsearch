{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
module Lib
  ( search,
  )
where

import Data.Traversable (for)
import Transformations.DataParsing (loadParsedData)
import Transformations.Extraction (executePipeline)
import Transformations.IndexOperations (buildInvertedIndex, InvertMap, find)
import Types.Index (ForwardIndex (ForwardIndex, document, words, links), InvertedIndex)
import Types.WebPageData (WebPageData (WebPageData))
import Types.WebPageDataProcessed (WebPageDataProcessed (WebPageDataProcessed), words)
import qualified Types.WebPageDataProcessed as WebPageDataProcessed

search = do
  loadedWebPageData <- loadParsedData "./res/reduced.jl" :: IO [Maybe WebPageData]
  let res = map executePipeline loadedWebPageData :: [Maybe ForwardIndex]
  
  let forwardData = buildInvertedIndex res :: Maybe InvertMap
  print forwardData

  putStrLn "search:"
  searchWord <- getLine

  putStrLn "result:"
  print $ find searchWord forwardData

  -- let urls = fmap (\o -> Types.Index.words o) <$> res
  -- print (take 10 urls)

check :: Maybe WebPageDataProcessed -> [String]
check (Just d) = Types.WebPageDataProcessed.words d
check Nothing = undefined
