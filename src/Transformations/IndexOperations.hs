{-# LANGUAGE OverloadedStrings #-}

module Transformations.IndexOperations (buildInvertedIndex, InvertMap, find) where

import qualified Data.Map as Map
import Data.Text (pack, unpack)
import qualified Data.Text as Data
import qualified Data.Text as Data.Text.Internal
import qualified Data.Text as T
import Text.HTML.Scalpel (scrapeStringLike, tagSelector, textSelector, texts)
import Transformations.InvertedIndex (InvertMap, transform)
import Types.Index
  ( ForwardIndex (ForwardIndex),
    InvertedIndex (InvertedIndex, documents, word),
  )
import qualified Types.Index as Index
import qualified Types.Index as Index.ForwardIndex

stripMaybe :: [Maybe ForwardIndex] -> [ForwardIndex]
stripMaybe ((Just x) : xs) = x : stripMaybe xs
stripMaybe (Nothing : xs) = stripMaybe xs
stripMaybe [] = []

buildInvertedIndex :: [Maybe ForwardIndex] -> Maybe InvertMap
buildInvertedIndex xs = Just (transform (stripMaybe xs) Map.empty)

find :: String -> Maybe InvertMap -> Maybe [String]
find str (Just map) = Map.lookup str map
find str Nothing = Nothing