{-# LANGUAGE OverloadedStrings #-} 

module DownloadHtml where 

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL 
import Network.HTTP.Simple

import Postdata 

action :: String
action = "https://erogamescape.dyndns.org/~ap2/ero/toukei_kaiseki/sql_for_erogamer_form.php"

userAgent :: B.ByteString
userAgent = "Mozilla/5.0 (en-US) Firefox/2.0.0.6667" 


getHtml :: String -> IO String
getHtml path = readFile path >>= fmap (T.unpack . T.decodeUtf8) . postSql action . PostSql   

postSql :: String -> PostSql -> IO B.ByteString
postSql url query = do

  request' <- parseRequest url
  let 
      baseRequest
        = setRequestMethod "POST"
        $ setRequestSecure True
        $ setRequestPort 443
        $ setRequestHeader "User-Agent" [userAgent]
        $ request'
      requestWithBody = setRequestBodyURLEncoded [("sql", B.pack (sql query))] baseRequest
  getResponseBody <$> httpBS requestWithBody
