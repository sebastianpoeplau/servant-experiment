{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module API
    ( startApp
    ) where

import           ClassyPrelude
import           Control.Monad.Logger       (runStderrLoggingT)
import           Control.Monad.Trans.Either (EitherT)
import           Database.Persist           ((==.))
import           Database.Persist.Class     (insertUnique, selectList)
import           Database.Persist.Sql       (SqlBackend, runMigration,
                                             runSqlConn)
import           Database.Persist.Sqlite    (withSqliteConn)
import           Database.Persist.Types     (Entity (entityVal))
import           Network.Wai                (Application)
import           Network.Wai.Handler.Warp   (run)
import           Servant

import           Scrape
import           Types


-- | This is our API, described on the type level.
type API = "articles" :> Get '[JSON] [Article]
      :<|> "getContents" :> ReqBody '[JSON] [Text] :> Post '[JSON] Text


-- | All functions handling API requests run in this monad.
type Handler = EitherT ServantErr IO


-- | Main entry point for running the API. It spawns a server on port 8080.
startApp :: IO ()
startApp = runStderrLoggingT $ withSqliteConn ":memory:" $ \connection -> do
  runSqlConn (runMigration migrateAll) connection
  liftIO $ run 8080 (app connection)


-- | A WAI application serving our API.
app :: SqlBackend -> Application
app connection = serve (Proxy :: Proxy API) (server connection)


-- | The server collects the actual implementations of each API function.
server :: SqlBackend -> Server API
server connection = getArticlesHandler connection
               :<|> getContentsHandler connection


-- | Handler for GET requests to "/articles"
getArticlesHandler :: SqlBackend -> Handler [Article]
getArticlesHandler connection = liftIO (updateArticleDB >> getUnreadArticles)
  where
    updateArticleDB = do
      articles <- getArticles
      (flip runSqlConn) connection $ forM_ articles (void . insertUnique)
    -- TODO: This includes all DB fields, including "downloaded". We should have a separate type with reduced information.
    getUnreadArticles = runSqlConn (map entityVal <$> selectList [ArticleDownloaded ==. False] []) connection


-- | Handler for POST requests to "/getContents".
getContentsHandler :: SqlBackend -> [Text] -> Handler Text
getContentsHandler connection urls = return ""
