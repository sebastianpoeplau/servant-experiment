{-# LANGUAGE OverloadedStrings #-}
module Scrape (getArticles, getArticleContents, Article(..)) where

import           Data.Maybe
import           Data.Text         (Text)
import qualified Data.Text         as T
import           Flow
import           Network.HTTP      (getRequest, getResponseBody, simpleHTTP)
import           Text.HTML.TagSoup

data Article = Article { articleTitle :: Text
                       , articleUrl   :: Text
                       } deriving (Show, Eq)


-- | Download a web page.
getPage :: Text -> IO Text
getPage url = do
    response <- url |> T.unpack .> getRequest .> simpleHTTP
    T.pack <$> getResponseBody response


-- | Extract a list of articles from Bunte.
getArticles :: IO [Article]
getArticles = do
    page <- getPage "http://www.bunte.de/"
    page |> parseTags .> mapMaybe toArticle .> return
    where
        toArticle (TagOpen "article" attrs) = Article <$> lookup "data-article-title" attrs
                                                      <*> lookup "data-internal-url"  attrs
        toArticle _                         = Nothing

getArticleContents :: Article -> IO Text
getArticleContents (Article _ url) = do
    page <- getPage url
    page |> parseTags
         .> dropWhile (~/= ("<div class=\"article-text text-html-content container-fluid\">" :: String))
         .> takeWhile (~/= ("<div id=likegate>" :: String))
         .> innerText
         .> T.lines .> filter (not . T.isPrefixOf "Im Video:") .> T.unlines
         .> T.words .> T.unwords -- removes excess whitespace
         .> return
