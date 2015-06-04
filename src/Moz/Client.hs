module Moz.Client
  ( Error
  , mozGet
  ) where

import Control.Arrow (left)
import Data.Aeson (FromJSON(..), eitherDecode)
import Data.List (intercalate)
import Data.Time.Clock (getCurrentTime)

import Network.HTTP.Types
import Network.HTTP.Conduit

import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

import Moz.Auth (Auth(..), expires, signature)

type MozQueryParam = SimpleQueryItem

type URL = String

data Error = HTTPConnectionError E.SomeException
           | JSONError String
           deriving (Show)

buildQueryString :: [MozQueryParam] -> BS.ByteString
buildQueryString params = renderSimpleQuery True params

buildUrl :: [String] -> URL
buildUrl paths = "http://lsapi.seomoz.com/" ++ intercalate "/" paths

makeRequest :: Method -> URL -> [MozQueryParam] -> IO (Either Error (Response LBS.ByteString))
makeRequest reqMethod url params = do
  request' <- parseUrl url
  let request = request' { method = reqMethod
                         , queryString = buildQueryString params
                         }
  ((withManager $ httpLbs request) >>= return . Right) `E.catch`
    (return . Left . HTTPConnectionError)

authParams :: Auth -> IO [MozQueryParam]
authParams auth = do
  now <- getCurrentTime
  return [ ("AccessID", accessId auth)
         , ("Expires", expires now)
         , ("Signature", signature auth now)
         ]

decodeResponse :: (FromJSON a) => LBS.ByteString -> Either Error a
decodeResponse json = left JSONError (eitherDecode json)

mozAPI :: (FromJSON a) => Auth -> Method -> URL -> [MozQueryParam] -> IO (Either Error a)
mozAPI auth reqMethod url queryParams = do
  aps <- authParams auth
  response <- makeRequest reqMethod url (queryParams ++ aps)
  return $ response >>= (decodeResponse . responseBody)

mozGet :: (FromJSON a) => Auth -> [String] -> [MozQueryParam] -> IO (Either Error a)
mozGet auth paths queryParams = mozAPI auth methodGet url queryParams
  where url = buildUrl paths
