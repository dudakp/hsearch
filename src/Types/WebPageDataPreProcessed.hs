module Types.WebPageDataPreProcessed
  ( WebPageDataPreProcessed (..),
    clone,
  )
where

import Data.Text (Text)
import qualified Types.WebPageDataProcessed as Types

data WebPageDataPreProcessed = WebPageDataPreProcessed
  { url :: String,
    words :: [Text],
    links :: [Text]
  }
  deriving (Show)

-- clone :: Types.WebPageDataPreProcessed -> WebPageDataPreProcessed
clone :: WebPageDataPreProcessed -> [Text] -> Maybe WebPageDataPreProcessed
clone d s =
  Just
    WebPageDataPreProcessed
      { Types.WebPageDataPreProcessed.url = Types.WebPageDataPreProcessed.url d,
        Types.WebPageDataPreProcessed.words = s,
        Types.WebPageDataPreProcessed.links = Types.WebPageDataPreProcessed.links d
      }