{-# LANGUAGE OverloadedStrings #-}

module Transformations.Extraction (executePipeline) where

import Control.Applicative (Alternative (empty))
import Data.String (fromString)
import Data.Text (pack, unpack)
import qualified Data.Text as Data
import qualified Data.Text as Data.Text.Internal
import qualified Data.Text as T
import GHC.TypeLits (ErrorMessage (Text))
import Text.HTML.Scalpel (scrapeStringLike, tagSelector, textSelector, texts)
import Text.Regex (mkRegex, subRegex)
import Types.WebPageData (WebPageData (WebPageData, htmlContent, url))
import Types.WebPageDataPreProcessed
  ( WebPageDataPreProcessed (WebPageDataPreProcessed, url, words),
  )
import Types.WebPageDataProcessed (WebPageDataProcessed (WebPageDataProcessed, words), url)
import Types.Index (ForwardIndex (ForwardIndex))
import qualified Types.Index as Index
import qualified Types.Index as Index.ForwardIndex
import Data.List (nub)

executePipeline :: Maybe WebPageData -> Maybe Index.ForwardIndex 
executePipeline = cleanData . filterData . splitData . stripData . parseHtml

parseHtml :: Maybe WebPageData -> Maybe WebPageDataPreProcessed
-- pattern matching - pokial je hodnota v Maybe argumente Just tak robim toto
parseHtml (Just d) = do
  scraped <- scrapeStringLike (htmlContent d) (texts (tagSelector "div")) :: Maybe [T.Text]
  let pageUrl = unpack (Types.WebPageData.url d) :: String
  return
    ( WebPageDataPreProcessed
        { Types.WebPageDataPreProcessed.url = pageUrl,
          Types.WebPageDataPreProcessed.words = scraped
        }
    )
-- pokial je to Noting tak robim toto
parseHtml Nothing = undefined

splitData :: Maybe WebPageDataPreProcessed -> Maybe WebPageDataPreProcessed
splitData (Just d) = do
  let splitted = concatMap (T.splitOn " ") (Types.WebPageDataPreProcessed.words d)
  return
    ( WebPageDataPreProcessed
        { Types.WebPageDataPreProcessed.url = Types.WebPageDataPreProcessed.url d,
          Types.WebPageDataPreProcessed.words = splitted
        }
    )
splitData Nothing = undefined

stripData :: Maybe WebPageDataPreProcessed -> Maybe WebPageDataPreProcessed
stripData (Just d) = do
  let stripped = map T.strip (Types.WebPageDataPreProcessed.words d)
  return
    ( WebPageDataPreProcessed
        { Types.WebPageDataPreProcessed.url = Types.WebPageDataPreProcessed.url d,
          Types.WebPageDataPreProcessed.words = stripped
        }
    )
stripData Nothing = undefined

filterData :: Maybe WebPageDataPreProcessed -> Maybe WebPageDataPreProcessed
filterData (Just d) = do
  let filtered = filter (\x -> T.length x > 0) (Types.WebPageDataPreProcessed.words d)
  return
    ( WebPageDataPreProcessed
        { Types.WebPageDataPreProcessed.url = Types.WebPageDataPreProcessed.url d,
          Types.WebPageDataPreProcessed.words = filtered
        }
    )
filterData Nothing = undefined

cleanData :: Maybe WebPageDataPreProcessed -> Maybe ForwardIndex 
cleanData (Just d) = do
  let converted = map T.unpack (Types.WebPageDataPreProcessed.words d)
  let cleaned = map (\w -> subRegex (mkRegex "<[^>]*>") w "") converted
  return
    ( ForwardIndex 
        { Index.ForwardIndex.document = Types.WebPageDataPreProcessed.url d,
          Index.ForwardIndex.words = nub cleaned
        }
    )
cleanData Nothing = undefined
