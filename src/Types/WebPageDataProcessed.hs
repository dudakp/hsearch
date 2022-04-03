module Types.WebPageDataProcessed
  ( WebPageDataProcessed (..),
  )
where

data WebPageDataProcessed = WebPageDataProcessed
  { url :: String,
    words :: [String]
  }
  deriving (Show)