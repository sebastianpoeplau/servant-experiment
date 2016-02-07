{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           ClassyPrelude
import           Test.Hspec

import           Scrape
import           Types


main :: IO ()
main = hspec $
    describe "Scrape" $ do
        describe "getArticles" $ do
            it "produces a non-empty list of articles" $
                getArticles `shouldNotReturn` []

            it "creates article URLs below bunte.de" $
                let shouldBeAtBunte (Article _ url _) = unpack url `shouldStartWith` "http://www.bunte.de/" in
                getArticles >>= mapM_ shouldBeAtBunte

        describe "getArticleContents" $
            it "returns a non empty text" $ do
                articles <- getArticles
                case articles of
                    [] -> expectationFailure "No articles available"
                    (a:_) -> getArticleContents a `shouldNotReturn` ""
