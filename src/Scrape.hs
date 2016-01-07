{-# LANGUAGE OverloadedStrings #-}
module Scrape (getArticles) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe
import Flow
import Network.HTTP (getRequest, getResponseBody, simpleHTTP)
import Text.HTML.TagSoup


data Article = Article { title :: Text
                       , url :: Text
                       }


-- | Download a web page.
getPage :: Text -> IO Text
getPage url = do
    response <- url |> T.unpack .> getRequest .> simpleHTTP
    T.pack <$> getResponseBody response


-- | Extract a list of articles from Bunte.
getArticles :: IO [Article]
getArticles = do
    page <- getPage "www.bunte.de"
    page |> parseTags .> filter (~== ("<article>" :: String)) .> map toArticle .> catMaybes .> return
    where
        toArticle (TagOpen _ attrs) = Article <$> lookup "data-article-title" attrs
                                              <*> lookup "data-internal-url" attrs
        toArticle _                 = Nothing
