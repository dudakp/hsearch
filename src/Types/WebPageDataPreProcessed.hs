module Types.WebPageDataPreProcessed
  ( WebPageDataPreProcessed (..),
    clone,
  )
where

import Data.Text (Text)

data WebPageDataPreProcessed = WebPageDataPreProcessed
  { url :: String,
    words :: [Text]
  }
  deriving (Show)

-- clone :: Types.WebPageDataPreProcessed -> WebPageDataPreProcessed
clone :: WebPageDataPreProcessed -> [Text] -> Maybe WebPageDataPreProcessed
clone d s =
  Just
    WebPageDataPreProcessed
      { Types.WebPageDataPreProcessed.url = Types.WebPageDataPreProcessed.url d,
        Types.WebPageDataPreProcessed.words = s
      }