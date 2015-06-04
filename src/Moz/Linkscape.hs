module Moz.Linkscape
  ( urlMetrics
  ) where

import qualified Data.ByteString.Char8 as C8

import Moz.Auth (Auth(..))
import Moz.Client (Error, mozGet)
import Moz.Linkscape.URLMetrics (URLMetrics, URLMetricCol(..), sumUrlMetricCols)

urlMetrics :: Auth -> String -> [URLMetricCol] -> IO (Either Error URLMetrics)
urlMetrics auth url metrics = mozGet auth ["linkscape", "url-metrics", url] [("Cols", cols)]
  where cols = C8.pack . show . sumUrlMetricCols $ metrics
