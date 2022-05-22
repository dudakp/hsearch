{-# LANGUAGE OverloadedStrings #-}

module Transformations.Extraction (executePipeline) where

import Control.Applicative (Alternative (empty))
import Data.Char
import Data.List (nub)
import Data.String (fromString)
import Data.Text (pack, unpack)
import qualified Data.Text as Data
import qualified Data.Text as Data.Text.Internal
import qualified Data.Text as T
import GHC.TypeLits (ErrorMessage (Text))
import qualified GHC.TypeLits as T
import Text.HTML.Scalpel (anySelector, attr, attrs, scrapeStringLike, tagSelector, textSelector, texts)
import Text.Regex (mkRegex, subRegex)
import Types.Index (ForwardIndex (ForwardIndex))
import qualified Types.Index as Index
import qualified Types.Index as Index.ForwardIndex
import Types.WebPageData (WebPageData (WebPageData, htmlContent, url))
import Types.WebPageDataPreProcessed
  ( WebPageDataPreProcessed (WebPageDataPreProcessed, links, url, words),
  )
import qualified Types.WebPageDataPreProcessed as Types
import Types.WebPageDataProcessed (WebPageDataProcessed (WebPageDataProcessed, words), url)

executePipeline :: Maybe WebPageData -> Maybe Index.ForwardIndex
executePipeline = makeForwardIndex . cleanData . filterData . splitData . stripData . parseHtml

parseHtml :: Maybe WebPageData -> Maybe WebPageDataPreProcessed
-- pattern matching - pokial je hodnota v Maybe argumente Just tak robim toto
parseHtml (Just d) = do
  scrapedParagraphs <- scrapeStringLike (htmlContent d) (texts "p")
  scrapedAs <- scrapeStringLike (htmlContent d) (texts "a")
  scrapedSpans <- scrapeStringLike (htmlContent d) (texts "span")
  scrapedHeaders <- scrapeStringLike (htmlContent d) (texts "h1")
  scrapedHeaders2 <- scrapeStringLike (htmlContent d) (texts "h2")
  links <- scrapeStringLike (htmlContent d) (attrs "href" anySelector)
  let pageUrl = unpack (Types.WebPageData.url d) :: String
  return
    ( WebPageDataPreProcessed
        { Types.WebPageDataPreProcessed.url = pageUrl,
          Types.WebPageDataPreProcessed.words = scrapedParagraphs ++ scrapedAs ++ scrapedSpans ++ scrapedHeaders ++ scrapedHeaders2,
          Types.WebPageDataPreProcessed.links = processLinks links
        }
    )
-- pokial je to Noting tak robim toto
parseHtml Nothing = Nothing

splitData :: Maybe WebPageDataPreProcessed -> Maybe WebPageDataPreProcessed
splitData (Just d) = do
  let splitted = concatMap (T.splitOn " ") (Types.WebPageDataPreProcessed.words d)
  Types.clone d splitted
splitData Nothing = Nothing

stripData :: Maybe WebPageDataPreProcessed -> Maybe WebPageDataPreProcessed
stripData (Just d) = do
  let stripped = map T.strip (Types.WebPageDataPreProcessed.words d)
  Types.clone d stripped
stripData Nothing = Nothing

filterData :: Maybe WebPageDataPreProcessed -> Maybe WebPageDataPreProcessed
filterData (Just d) = do
  let filtered = filter (\x -> T.length x > 0) (Types.WebPageDataPreProcessed.words d)
  Types.clone d filtered
filterData Nothing = Nothing

cleanData :: Maybe WebPageDataPreProcessed -> Maybe WebPageDataPreProcessed
cleanData (Just d) = do
  let lower = map T.toLower (Types.WebPageDataPreProcessed.words d)
  let converted = map T.unpack lower
  let cleaned = nub (map (\w -> subRegex (mkRegex "<[^>]*>") w "") converted)
  let alphanumeric = map (filter (\l -> l `elem` ['a' .. 'z'] ++ ['0' .. '9'])) cleaned
  Types.clone d (map T.pack alphanumeric)
cleanData Nothing = Nothing

processLinks :: [T.Text] -> [T.Text]
processLinks d = do
  filter (T.isPrefixOf "http") (filter (\l -> T.length l > 1) (map T.strip d))

makeForwardIndex :: Maybe WebPageDataPreProcessed -> Maybe ForwardIndex
makeForwardIndex (Just d) = do
  return
    ( ForwardIndex
        { Index.ForwardIndex.document = Types.WebPageDataPreProcessed.url d,
          Index.ForwardIndex.words = map T.unpack (Types.WebPageDataPreProcessed.words d),
          Index.ForwardIndex.links = map T.unpack (Types.WebPageDataPreProcessed.links d)
        }
    )
makeForwardIndex Nothing = Nothing