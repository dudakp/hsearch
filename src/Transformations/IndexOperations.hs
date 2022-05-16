{-# LANGUAGE OverloadedStrings #-}

module Transformations.IndexOperations (buildInvertedIndex, InvertMap, find) where

import Types.Index (InvertedIndex (InvertedIndex, documents, word))
import qualified Data.Text as Data
import qualified Data.Text as Data.Text.Internal
import qualified Data.Text as T
import Data.Text (pack, unpack)
import Types.Index (ForwardIndex (ForwardIndex))
import qualified Types.Index as Index
import qualified Types.Index as Index.ForwardIndex
import Text.HTML.Scalpel (scrapeStringLike, tagSelector, textSelector, texts)
import Transformations.InvertedIndex (transform, InvertMap)
import qualified Data.Map as Map


stripMaybe :: [Maybe ForwardIndex] -> [ForwardIndex]
stripMaybe ((Just x):xs) = x : stripMaybe xs
stripMaybe ((Nothing):xs) = stripMaybe xs
stripMaybe [] = []

buildInvertedIndex :: [Maybe ForwardIndex] -> Maybe InvertMap
buildInvertedIndex xs =  Just (transform (stripMaybe xs) Map.empty)


find :: String -> Maybe InvertMap -> Maybe [String]
find str (Just map) = Map.lookup str map
find str Nothing = Nothing