{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
module Lib
  ( search,
  )
where

import qualified Data.Maybe
import Data.Traversable (for)
import Transformations.DataParsing (loadParsedData)
import Transformations.Extraction (executePipeline)
import Transformations.IndexOperations (InvertMap, buildInvertedIndex, find)
import Types.Index (ForwardIndex (ForwardIndex, document, links, words), InvertedIndex)
import Types.WebPageData (WebPageData (WebPageData))
import Types.WebPageDataProcessed (WebPageDataProcessed (WebPageDataProcessed), words)
import qualified Types.WebPageDataProcessed as WebPageDataProcessed

search :: IO ()
search = do
  loadedWebPageData <- loadParsedData "./res/larget.txt" :: IO [Maybe WebPageData]
  let res = map executePipeline loadedWebPageData :: [Maybe ForwardIndex]
  let forwardData = buildInvertedIndex res :: Maybe InvertMap

  putStrLn "search:"
  searchWord <- getLine

  let searchResult = find searchWord forwardData
  if Data.Maybe.isJust searchResult
    then do
      putStrLn "result:"
      print searchResult
    else print "Term not found!"

check :: Maybe WebPageDataProcessed -> [String]
check (Just d) = Types.WebPageDataProcessed.words d
check Nothing = undefined
