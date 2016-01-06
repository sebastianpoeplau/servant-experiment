module Scrape () where

import Network.HTTP (getRequest, getResponseBody, simpleHTTP)


-- | Download a web page.
getPage :: String -> IO String
getPage url = simpleHTTP (getRequest url) >>= getResponseBody
