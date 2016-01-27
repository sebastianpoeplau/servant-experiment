{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module API
    ( startApp
    ) where

import           Control.Monad.IO.Class
import           Control.Monad.Logger     (runStderrLoggingT)
import           Database.Persist.Sql     (SqlBackend, runMigration, runSqlConn)
import           Database.Persist.Sqlite  (withSqliteConn)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

import           Scrape
import           Types


type API = "articles" :> Get '[JSON] [Article]

startApp :: IO ()
startApp = runStderrLoggingT $ withSqliteConn ":memory:" $ \connection -> do
  runSqlConn (runMigration migrateAll) connection
  liftIO $ run 8080 (app connection)

app :: SqlBackend -> Application
app connection = serve (Proxy :: Proxy API) (server connection)

server :: SqlBackend -> Server API
server = getArticlesHandler

getArticlesHandler connection = liftIO getArticles
