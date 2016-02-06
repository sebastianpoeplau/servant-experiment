{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text  as T
import qualified Data.Text.IO as T
import           Test.Hspec

import           Scrape
import           Types


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
                T.putStrLn contents
                contents `shouldNotBe` ""
