
module Types.WebPageDataPreProcessed
  ( WebPageDataPreProcessed (..),
  )

where
import Data.Text (Text)

data WebPageDataPreProcessed = WebPageDataPreProcessed
  { url :: String,
    words :: [Text]
  }
  deriving (Show)