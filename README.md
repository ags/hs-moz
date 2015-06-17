moz
---

A Haskell client for the Mozscape API.

Currently, the client only supports the `url-metrics` endpoint.

This is not published on hackage and shouldn't be considered production ready
yet.

## Example Usage

```haskell
import Moz.Auth (Auth(..))
import Moz.Linkscape.URLMetrics (URLMetricCol(..))
import qualified Moz.Linkscape as LS

main :: IO ()
main = do
  metrics <- LS.runMozT auth $ do
    LS.urlMetrics "moz.com" testCols
  print metrics
  where auth = Auth "access-id-123" "secret-key-456"
        cols = [ CanoncialURL
               , DomainAuthority
               , PageAuthority
               , MozRankURL
               ]
```
