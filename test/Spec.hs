{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Scrape
import Test.Hspec


main :: IO ()
main = hspec $ do
    describe "Scrape.getArticles" $ do
        it "produces a non-empty list of articles" $ do
            getArticles >>= (`shouldSatisfy` not . null)

        it "creates article URLs below bunte.de" $ do
            getArticles >>= mapM_ shouldBeAtBunte
            where
                shouldBeAtBunte (Article _ url) = url `shouldSatisfy` ("http://www.bunte.de/" `T.isPrefixOf`)
