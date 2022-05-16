{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Transformations.InvertedIndex (transform, InvertMap) where 

import qualified Data.Map as Map
import Types.Index (ForwardIndex (ForwardIndex), InvertedIndex (InvertedIndex))
import Data.List (nub)

type InvertMap = Map.Map String [String]
emptyMap = Map.empty

transformMap :: ForwardIndex -> InvertMap -> InvertMap
transformMap (ForwardIndex url [word] _) oldMap = newMap where 
    newMap = if Map.member word oldMap then Map.update (updateMap url) word oldMap else Map.insert word [url] oldMap

transformMap (ForwardIndex url (word:rest) links) oldMap = transformMap (ForwardIndex url rest links) newMap where 
    newMap = if Map.member word oldMap then Map.update (updateMap url) word oldMap else Map.insert word [url] oldMap

updateMap :: String -> [String] -> Maybe [String]
updateMap url urls = Just (nub (url:urls))
updateMap url [] = Just (url:[])

transform :: [ForwardIndex] -> InvertMap -> InvertMap 
transform [index] oldMap = transformMap index oldMap 
transform (index:rest) oldMap = transform rest newMap where 
    newMap = transformMap index oldMap

