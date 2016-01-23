{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module API
    ( startApp
    ) where

import           Control.Monad.IO.Class
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

import           Scrape
import           Types


type API = "articles" :> Get '[JSON] [Article]

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = liftIO getArticles
