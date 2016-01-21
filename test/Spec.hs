{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Scrape
import Test.Hspec


main :: IO ()
main = hspec $
    describe "Scrape" $ do
        describe "getArticles" $ do
            it "produces a non-empty list of articles" $
                getArticles >>= shouldNotBe []

            it "creates article URLs below bunte.de" $
                let shouldBeAtBunte (Article _ url) = T.unpack url `shouldStartWith` "http://www.bunte.de/" in
                getArticles >>= mapM_ shouldBeAtBunte

        describe "getArticleContents" $
            it "returns a non empty text" $ do
                contents <- head <$> getArticles >>= getArticleContents
                print contents
                contents `shouldNotBe` ""

