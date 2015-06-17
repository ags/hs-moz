module Moz.Linkscape
  ( urlMetrics
  , runMozT
  ) where

import Control.Monad.Trans (MonadIO)

import qualified Data.ByteString.Char8 as C8

import Moz.Client (MozT, mozGet, runMozT)
import Moz.Linkscape.URLMetrics (URLMetrics(..), URLMetricCol(..), sumUrlMetricCols)

urlMetrics :: MonadIO m => String -> [URLMetricCol] -> MozT m URLMetrics
urlMetrics url metrics = mozGet ["linkscape", "url-metrics", url] [("Cols", cols)]
  where cols = C8.pack . show . sumUrlMetricCols $ metrics
