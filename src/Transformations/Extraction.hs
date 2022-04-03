module Transformations.Extraction (processData) where

import qualified Data.Text as Data.Text.Internal
import Text.HTML.Scalpel (scrapeStringLike, tagSelector, textSelector, texts)
import Types.WebPageData (WebPageData (WebPageData, htmlContent))

processData :: Maybe WebPageData -> Maybe [Data.Text.Internal.Text]
processData (Just d) = scrapeStringLike (htmlContent d) (texts (tagSelector "div"))
processData Nothing = undefined