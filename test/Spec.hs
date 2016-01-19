{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Scrape
import Test.Hspec
import qualified Data.Text.IO as TI
import Text.Regex.Posix


main :: IO ()
main = hspec $
    describe "Scrape" $ do
        describe "getArticles" $ do
            it "produces a non-empty list of articles" $
                getArticles >>= (`shouldSatisfy` not . null)

            it "creates article URLs below bunte.de" $
                let shouldBeAtBunte (Article _ url) = url `shouldSatisfy` ("http://www.bunte.de/" `T.isPrefixOf`) in
                getArticles >>= mapM_ shouldBeAtBunte

        describe "getArticleContents" $
            it "returns a non empty text" $ do
                contents <- head <$> getArticles >>= getArticleContents
                print contents
                contents `shouldSatisfy` not . T.null

