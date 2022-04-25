module Types.Index
  ( InvertedIndex (..),
    ForwardIndex (..),
  )
where

data InvertedIndex = InvertedIndex
  { word :: String,
    documents :: [String]
  }
  deriving (Show)

data ForwardIndex = ForwardIndex
  { document :: String,
    words :: [String]
  }
  deriving (Show)
