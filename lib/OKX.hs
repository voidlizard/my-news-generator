module OKX where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.JsonStream.Parser
import Data.Data
import Data.Text qualified as Text
import Data.Time.Clock.POSIX

import Data.Facts.Common
import Data.Facts.Currencies

data OKX

deriving stock instance Data OKX
deriving stock instance Data (MarketData OKX)


data instance Fact (MarketData OKX) =
  OKXMarketData
  { okxFactPair  :: CurrencyPair Symbol
  , okxRate      :: ExchangeRate E6
  , okxOpen24h   :: ExchangeRate E6
  , okxTimestamp :: POSIXTime
  }
  deriving stock (Eq,Ord,Data,Show)


type OKXMarketFact = Fact (MarketData OKX)


posixFromMillis :: Integral x => x -> POSIXTime
posixFromMillis n = fromIntegral a + fromIntegral b /1000
  where (a,b) = n `divMod` 1000

marketDataParser :: Parser [Fact (MarketData OKX)]
marketDataParser = "data" .: many ( arrayOf entry )
  where
    entry = OKXMarketData <$> pair <*> rate <*> open24 <*> ts
    pair  = CurrencyPair <$> symbol "ccy" <*> pure "USD"
    symbol x = Symbol <$> x .: string
    rate  = read . Text.unpack <$> "last" .: string
    open24 = read . Text.unpack <$> "open24h" .: string
    ts = posixFromMillis @Integer . read . Text.unpack <$> ( "ts" .: string )

-- from where? raw file, http request, proxy-shmoxy, etc
-- what exactly? fxrate, volume,  24h change, etc
getMarketInfo :: IO [Fact (MarketData OKX)]
getMarketInfo = undefined

