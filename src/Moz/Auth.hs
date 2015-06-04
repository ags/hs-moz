{-# LANGUAGE OverloadedStrings #-}

module Moz.Auth
  ( Auth(..)
  , expires
  , signature
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C8

import Crypto.Hash.SHA1 (hash)
import Crypto.MAC.HMAC (hmac)

import Data.Time.Clock (UTCTime, addUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

data Auth = Auth { accessId :: BS.ByteString
                 , secretKey :: BS.ByteString
                 } deriving (Show)

toSeconds :: UTCTime -> Integer
toSeconds = round . utcTimeToPOSIXSeconds

expires :: UTCTime -> BS.ByteString
expires = C8.pack . show . toSeconds . addUTCTime 300

signature :: Auth -> UTCTime -> BS.ByteString
signature auth time = B64.encode . sign $ BS.intercalate "\n" [accessId auth, expires time]
  where sign = hmac hash 64 (secretKey auth)
