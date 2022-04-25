module Types.WebPageDataProcessed
  ( WebPageDataProcessed (..),
  )

where

import Data.Text (Text)

data WebPageDataProcessed = WebPageDataProcessed
  { url :: String,
    words :: [String]
  }
  deriving (Show)

