module Types.WebPageData
  ( WebPageData (..),
  )
where

import Data.Text (Text)

data WebPageData = WebPageData
  { url :: Text,
    htmlContent :: Text
  }
  deriving (Show)
