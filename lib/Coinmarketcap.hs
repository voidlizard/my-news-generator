{-# LANGUAGE TemplateHaskell #-}
module Coinmarketcap where

import Data.Aeson.Types (Value(..))
import Control.Monad
import Control.Applicative
import Data.ByteString.Lazy (ByteString)
import Network.Curl.Download.Lazy
import Network.Curl.Opts
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.JsonStream.Parser
-- import Safe
import System.Environment
import System.Exit
import Data.Data
import Data.Time.Clock.POSIX
import Data.Time.Clock (UTCTime)
import Data.Time.Format
import Lens.Micro.TH

import Data.Facts.Common
import Data.Facts.Currencies

data CMC

deriving stock instance Data CMC
deriving stock instance Data (MarketData CMC)


data instance Fact (MarketData CMC) =
  CMCMarketData
  { _cmcPair      :: CurrencyPair Symbol
  , _cmcName      :: Text
  , _cmcSlug      :: Text
  , _cmcRate      :: ExchangeRate E6
  , _cmcChange24h :: ExchangeRate E6
  , _cmcTimestamp :: POSIXTime
  }
  deriving stock (Eq,Ord,Data,Show)

makeLenses 'CMCMarketData


type CMCMarketFact = Fact (MarketData CMC)

downloadMarketInfo :: IO ByteString
downloadMarketInfo = do
  apiKeyFile <- lookupEnv "CMC_PRO_API_KEY_FILE" >>= maybe (die "CMC_PRO_API_KEY_FILE not set") pure
  apiKey <- Text.unpack . Text.strip <$> Text.readFile apiKeyFile

  let req = "https://pro-api.coinmarketcap.com/v2/cryptocurrency/quotes/latest?symbol=BTC,ETH,TON"
  let headers = [ CurlHttpHeaders [ "Accept: application/json"
                                  , "X-CMC_PRO_API_KEY: " <> apiKey
                                  ]
                ]

  let opts = headers
  openLazyURIWithOpts opts req >>= either (die.show) pure


marketDataParser :: Parser (Fact (MarketData CMC))
marketDataParser = "data" .: objectValues (arrayOf fact)
  where
    fact = CMCMarketData <$> pair <*> name <*> slug <*> rate <*> change24 <*> ts
    symbol x = Symbol <$> x .: string
    pair  = CurrencyPair <$> symbol "symbol" <*> pure "USD"
    name = "name" .: string
    slug = "slug" .: string
    rate = realToFrac <$> "quote" .: "USD" .: "price" .: number
    change24 = realToFrac <$> "quote" .: "USD" .: "percent_change_24h" .: number
    ts = "quote" .: "USD" .: "last_updated" .: posix

    posix = valueWith $ \case
      String s -> do
        x <- parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%Z" (Text.unpack s)
        pure $ utcTimeToPOSIXSeconds x

      _        -> mzero

-- t <- parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%Z" "2022-12-23T20:51:00.00Z" :: IO UTCTime
-- parseByteString ( "data" .: objectValues  ( ( arrayOf ( (,,) <$> "symbol" .: string <*> "name" .: string  <*> "quote" .: "USD" .: "price" .: number   ) )  )   )  s
-- marketDataParser :: Parser [Fact (MarketData CMC)]
