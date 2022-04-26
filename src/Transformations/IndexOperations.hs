module Transformations.IndexOperations (buildInvertedIndex) where

import Types.Index (ForwardIndex, InvertedIndex (InvertedIndex, documents, word))

buildInvertedIndex :: Maybe ForwardIndex -> Maybe InvertedIndex
buildInvertedIndex (Just d) = do
  return
    ( InvertedIndex
        { word = "",
          documents = []
        }
    )
buildInvertedIndex Nothing = undefined
