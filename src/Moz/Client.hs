{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Moz.Client
  ( MozError
  , MozT(..)
  , mozGet
  , runMozT
  ) where

import Control.Monad.Reader
import Control.Monad.Except

import Data.Aeson (FromJSON(..), eitherDecode)
import Data.List (intercalate)
import Data.Time.Clock (getCurrentTime)

import Network.HTTP.Types
import Network.HTTP.Conduit

import qualified Control.Exception as E
import qualified Data.ByteString.Lazy.Char8 as LBS

import Moz.Auth (Auth(..), expires, signature)

type MozQueryParam = SimpleQueryItem

type URL = String

data MozError = HTTPConnectionError E.SomeException
              | InvalidRequest
              | JSONError String
              deriving (Show)

newtype MozT m a = MozT { unMozT :: ExceptT MozError (ReaderT Auth m) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader Auth
           , MonadError MozError
           )

runMozT :: MonadIO m => Auth -> MozT m a -> m (Either MozError a)
runMozT auth m = runReaderT (runExceptT (unMozT m)) auth

buildUrl :: [String] -> URL
buildUrl paths = "http://lsapi.seomoz.com/" ++ intercalate "/" paths

prepReq :: Method -> URL -> [MozQueryParam] -> Maybe Request
prepReq m url params = fmap (setM . setQS) (parseUrl url)
  where setM r = r { method = m }
        setQS r = r { queryString = renderSimpleQuery True params }

makeRequest :: (MonadIO m) => Request -> MozT m (Response LBS.ByteString)
makeRequest req = do
  rsp <- liftIO (((withManager $ httpLbs req) >>= return . Right) `E.catch` (return . Left))
  either (throwError . HTTPConnectionError) return rsp

authParams :: MonadIO m => MozT m [MozQueryParam]
authParams = do
  auth <- ask
  now <- liftIO $ getCurrentTime
  return [ ("AccessID", accessId auth)
         , ("Expires", expires now)
         , ("Signature", signature auth now)
         ]

mozAPI :: (FromJSON a, MonadIO m) => Method -> URL -> [MozQueryParam] -> MozT m a
mozAPI reqMethod url queryParams = do
  authP <- authParams
  req <- maybe (throwError InvalidRequest) return (prepReq reqMethod url (queryParams ++ authP))
  response <- makeRequest req
  either (throwError . JSONError) return (eitherDecode (responseBody response))

mozGet :: (FromJSON a, MonadIO m) => [String] -> [MozQueryParam] -> MozT m a
mozGet paths queryParams = mozAPI methodGet url queryParams
  where url = buildUrl paths
