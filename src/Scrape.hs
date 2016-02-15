{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Scrape (getArticles, getArticleContents) where

import           ClassyPrelude
import           Flow
import           Network.HTTP.Client
import           Text.HTML.TagSoup

import           Types


-- | Download a web page.
getPage :: Text -> IO Text
getPage url = do
    -- TODO: we shouldn't create a new manager each time
    manager <- newManager defaultManagerSettings
    request <- parseUrl (unpack url)
    response <- httpLbs request manager
    return . decodeUtf8 . toStrict . responseBody $ response


-- | Extract a list of articles from Bunte.
getArticles :: IO [Article]
getArticles = do
    page <- getPage "http://www.bunte.de/"
    page |> parseTags .> mapMaybe toArticle .> return
    where
        toArticle (TagOpen "article" attrs) = Article <$> lookup "data-article-title" attrs
                                                      <*> lookup "data-internal-url"  attrs
                                                      <*> pure False
        toArticle _                         = Nothing


-- | Extract the contents of an article from Bunte.
getArticleContents :: Article -> IO Text
getArticleContents (Article _ url _) = do
    page <- getPage url
    page |> parseTags
         .> dropWhile (~/= ("<div class=\"article-text text-html-content container-fluid\">" :: String))
         .> takeWhile (~/= ("<div id=likegate>" :: String))
         .> innerText
         .> lines .> filter (not . isPrefixOf "Im Video:") .> unlines
         .> words .> unwords -- removes excess whitespace
         .> return
