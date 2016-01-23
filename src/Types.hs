{-# LANGUAGE TemplateHaskell #-}
module Types where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)


data Article = Article { articleTitle :: Text
                       , articleUrl   :: Text
                       } deriving (Show, Eq)

$(deriveJSON defaultOptions ''Article)
