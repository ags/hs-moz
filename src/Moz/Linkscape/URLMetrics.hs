{-# LANGUAGE OverloadedStrings #-}

module Moz.Linkscape.URLMetrics
  ( URLMetrics(..)
  , URLMetricCol(..)
  , sumUrlMetricCols
  ) where

import Data.Aeson (FromJSON(..), Value(..), (.:?))
import Control.Monad (mzero)

data URLMetricCol = Title
                  | CanoncialURL
                  | Subdomain
                  | RootDomain
                  | ExternalEquityLinks
                  | SubdomainExternalLinks
                  | RootDomainExternalLinks
                  | EquityLinks
                  | SubdomainsLinking
                  | RootDomainsLinking
                  | Links
                  | SubdomainSubdomainsLinking
                  | RootDomainRootDomainsLinking
                  | MozRankURL
                  | MozRankSubdomain
                  | MozRankRootDomain
                  | MozTrust
                  | MozTrustSubdomain
                  | MozTrustRootDomain
                  | MozRankExternalEquity
                  | MozRankSubdomainExternalEquity
                  | MozRankRootDomainExternalEquity
                  | MozRankSubdomainCombined
                  | MozRankRootDomainCombined
                  | SubdomainSpamScore
                  | Social
                  | HTTPStatusCode
                  | LinksToSubdomain
                  | LinksToRootDomain
                  | RootDomainsLinkingToSubdomain
                  | PageAuthority
                  | DomainAuthority
                  | ExternalLinks
                  | ExternalLinksToSubdomain
                  | ExternalLinksToRootDomain
                  | LinkingCBlocks
                  | TimeLastCrawled
                  deriving (Enum, Eq, Show)

-- | Convert a list of URL metrics into a bit mask representing their sum.
sumUrlMetricCols :: [URLMetricCol] -> Int
sumUrlMetricCols = foldr (+) 0 . map toBitFlag

-- This looks like it should be able to be simplified by bitshifting using the
-- position of the value in the enum, but the bit flags have gaps which would
-- comprimise the data type, and the first one ('Title') doesn't fit the
-- pattern.
toBitFlag :: URLMetricCol -> Int
toBitFlag Title                           = 1
toBitFlag CanoncialURL                    = 4
toBitFlag Subdomain                       = 8
toBitFlag RootDomain                      = 16
toBitFlag ExternalEquityLinks             = 32
toBitFlag SubdomainExternalLinks          = 64
toBitFlag RootDomainExternalLinks         = 128
toBitFlag EquityLinks                     = 256
toBitFlag SubdomainsLinking               = 512
toBitFlag RootDomainsLinking              = 1024
toBitFlag Links                           = 2048
toBitFlag SubdomainSubdomainsLinking      = 4096
toBitFlag RootDomainRootDomainsLinking    = 8192
toBitFlag MozRankURL                      = 16384
toBitFlag MozRankSubdomain                = 32768
toBitFlag MozRankRootDomain               = 65536
toBitFlag MozTrust                        = 131072
toBitFlag MozTrustSubdomain               = 262144
toBitFlag MozTrustRootDomain              = 524288
toBitFlag MozRankExternalEquity           = 1048576
toBitFlag MozRankSubdomainExternalEquity  = 2097152
toBitFlag MozRankRootDomainExternalEquity = 4194304
toBitFlag MozRankSubdomainCombined        = 8388608
toBitFlag MozRankRootDomainCombined       = 16777216
toBitFlag SubdomainSpamScore              = 67108864
toBitFlag Social                          = 134217728
toBitFlag HTTPStatusCode                  = 536870912
toBitFlag LinksToSubdomain                = 4294967296
toBitFlag LinksToRootDomain               = 8589934592
toBitFlag RootDomainsLinkingToSubdomain   = 17179869184
toBitFlag PageAuthority                   = 34359738368
toBitFlag DomainAuthority                 = 68719476736
toBitFlag ExternalLinks                   = 549755813888
toBitFlag ExternalLinksToSubdomain        = 140737488355328
toBitFlag ExternalLinksToRootDomain       = 2251799813685248
toBitFlag LinkingCBlocks                  = 36028797018963968
toBitFlag TimeLastCrawled                 = 144115188075855872

data URLMetrics = URLMetrics { title :: Maybe String -- ut
                             , canonicalURL :: Maybe String -- uu
                             , subdomain :: Maybe String -- ufq
                             , rootDomain :: Maybe String -- upl
                             , externalEquityLinks :: Maybe Int -- ueid
                             , subdomainExternalLinks :: Maybe Int -- feid
                             , rootDomainExternalLinks :: Maybe Int -- peid
                             , equityLinks :: Maybe Int -- ujid
                             , subdomainsLinking :: Maybe Int -- uifq
                             , rootDomainsLinking :: Maybe Int -- uipl
                             , links :: Maybe Int -- uid
                             , subdomainSubdomainsLinking :: Maybe Int -- fid
                             , rootDomainRootDomainsLinking :: Maybe Int -- pid
                             , mozRankURLNormalized :: Maybe Double -- umrp
                             , mozRankURLRaw :: Maybe Double -- umrr
                             , mozRankSubdomainNormalized :: Maybe Double -- fmrp
                             , mozRankSubdomainRaw :: Maybe Double -- fmrr
                             , mozRankRootDomainNormalized :: Maybe Double -- pmrp
                             , mozRankRootDomainRaw :: Maybe Double -- pmrr
                             , mozTrustNormalized :: Maybe Double -- utrp
                             , mozTrustRaw :: Maybe Double -- utrr
                             , mozTrustSubdomainNormalized :: Maybe Double -- ftrp
                             , mozTrustRootDomainNormalized :: Maybe Double -- ptrp
                             , mozTrustRootDomainRaw :: Maybe Double -- ptrr
                             , mozRankExternalEquityNormalized :: Maybe Double -- uemrp
                             , mozRankExternalEquityRaw :: Maybe Double -- uemrr
                             , mozRankSubdomainExternalEquityNormalized :: Maybe Double -- fejp
                             , mozRankSubdomainExternalEquityRaw :: Maybe Double -- fejr
                             , mozRankRootDomainExternalEquityNormalized :: Maybe Double -- pejp
                             , mozRankRootDomainExternalEquityRaw :: Maybe Double -- pejr
                             , mozRankSubdomainCombinedNormalized :: Maybe Double -- pjp
                             , mozRankSubdomainCombinedRaw :: Maybe Double -- pjr
                             , mozRankRootDomainCombinedNormalized :: Maybe Double -- fjp
                             , mozRankRootDomainCombinedRaw :: Maybe Double -- fjr
                             , subdomainSpamScoreSubdomain :: Maybe Int -- fspsc
                             , subdomainSpamScoreFlags :: Maybe Int -- fspf
                             , subdomainSpamScoreLanguage :: Maybe String -- flan
                             , subdomainSpamScoreCrawlStatusCode :: Maybe Int -- fsps
                             , subdomainSpamScoreLastCrawled :: Maybe Int -- fsplc
                             , subdomainSpamScorePagesCrawled :: Maybe [String] -- fspp
                             , socialFacebookAccount :: Maybe String -- ffb
                             , socialTwitterHandle :: Maybe String -- ftw
                             , socialGooglePlusAccount :: Maybe String -- fg+
                             , socialEmailAddress :: Maybe String -- fem
                             , httpStatusCode :: Maybe String -- us
                             , linksToSubdomain :: Maybe Int -- fuid
                             , linksToRootDomain :: Maybe Int -- puid
                             , rootDomainsLinkingToSubdomain :: Maybe Int -- fipl
                             , pageAuthority :: Maybe Double -- upa
                             , domainAuthority :: Maybe Double -- pda
                             , externalLinks :: Maybe Int -- ued
                             , externalLinksToSubdomain :: Maybe Int -- fed
                             , externalLinksToRootDoamin :: Maybe Int -- ped
                             , linkingCBlocks :: Maybe Int -- pib
                             , timeLastCrawed :: Maybe Int -- ulc
                             } deriving (Show)

instance FromJSON URLMetrics where
  parseJSON (Object v) =
    URLMetrics <$> v .:? "ut"
               <*> v .:? "uu"
               <*> v .:? "ufq"
               <*> v .:? "upl"
               <*> v .:? "ueid"
               <*> v .:? "feid"
               <*> v .:? "peid"
               <*> v .:? "ujid"
               <*> v .:? "uifq"
               <*> v .:? "uipl"
               <*> v .:? "uid"
               <*> v .:? "fid"
               <*> v .:? "pid"
               <*> v .:? "umrp"
               <*> v .:? "umrr"
               <*> v .:? "fmrp"
               <*> v .:? "fmrr"
               <*> v .:? "pmrp"
               <*> v .:? "pmrr"
               <*> v .:? "utrp"
               <*> v .:? "utrr"
               <*> v .:? "ftrp"
               <*> v .:? "ptrp"
               <*> v .:? "ptrr"
               <*> v .:? "uemrp"
               <*> v .:? "uemrr"
               <*> v .:? "fejp"
               <*> v .:? "fejr"
               <*> v .:? "pejp"
               <*> v .:? "pejr"
               <*> v .:? "pjp"
               <*> v .:? "pjr"
               <*> v .:? "fjp"
               <*> v .:? "fjr"
               <*> v .:? "fspsc"
               <*> v .:? "fspf"
               <*> v .:? "flan"
               <*> v .:? "fsps"
               <*> v .:? "fsplc"
               <*> v .:? "fspp"
               <*> v .:? "ffb"
               <*> v .:? "ftw"
               <*> v .:? "fg+"
               <*> v .:? "fem"
               <*> v .:? "us"
               <*> v .:? "fuid"
               <*> v .:? "puid"
               <*> v .:? "fipl"
               <*> v .:? "upa"
               <*> v .:? "pda"
               <*> v .:? "ued"
               <*> v .:? "fed"
               <*> v .:? "ped"
               <*> v .:? "pib"
               <*> v .:? "ulc"
  parseJSON _ = mzero
