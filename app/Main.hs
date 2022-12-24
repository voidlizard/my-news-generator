{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.ByteString.Lazy.Char8 qualified as L8
import System.Environment
import Safe
import Network.Curl.Download.Lazy
import Network.Curl.Opts

import Control.Monad

import Text.InterpolatedString.Perl6 (qc)
import System.Directory
import System.IO

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text

import Data.Aeson

import Data.Fixed
import Safe

import Data.Data
import Data.Generics.Uniplate.Data()
import Data.Generics.Uniplate.Operations
import GHC.Generics

import Lens.Micro.Platform
import Lens.Micro.TH

import Prettyprinter


data Color = Red | Green

data Colored a = Colored Color a

newtype OKXMarketsData =
  OKXMarketsData { _okxMarketsData :: [Ins]
                 }
                 deriving (Eq,Ord,Data,Show)

data Ins = Ins { _insTicker  :: Text
               , _insLast    :: Fixed E2
               , _insOpen24h :: Fixed E2
               , _insTs      :: Int
               }
               deriving (Eq,Ord,Data,Show)


makeLenses ''Ins
makeLenses ''OKXMarketsData

instance FromJSON OKXMarketsData where
  parseJSON = withObject "OKXMarketData" $ \o ->
    OKXMarketsData <$> o .: "data"

instance FromJSON Ins where
  parseJSON = withObject "Ins" $ \v -> Ins
        <$> v .: "ccy"
        <*> (read <$> v .: "last")
        <*> (read <$> v .: "open24h")
        <*> (read <$> v .: "ts")


newtype AsShow a = AsShow a

instance Pretty (AsShow (Fixed E2)) where
  pretty (AsShow x) = pretty $ show x

instance Pretty Color where
  pretty Red = "#c00000"
  pretty Green = "#00c000"

instance Pretty a => Pretty (Colored a) where
  pretty (Colored c x) = "%{F" <> pretty c <> "}" <+> pretty x <> "%{F-}"


instance Pretty Ins where
  pretty ins = pretty (view insTicker ins)
               <> colon
               <> pretty (color $ AsShow $ view insLast ins)
    where
      color | view insLast ins < view insOpen24h ins = Colored Red
            | otherwise = Colored Green


main :: IO ()
main = do
  pure ()

  -- let proxyFile = "/home/dmz/dmz-data/proxy/65.108.61.217"
  -- proxyExists <- doesFileExist proxyFile

  -- proxyOpt <- if proxyExists then do
  --               txt <- Text.readFile proxyFile
  --               let o = Text.strip txt
  --               hPutStrLn stderr [qc|proxy set {o}|]
  --               pure [CurlProxy (Text.unpack o)]
  --             else do
  --               hPutStrLn stderr [qc|proxy not set|]
  --               pure []


  -- let req = "https://www.okx.com/priapi/v5/market/mult-cup-tickers?ccys=BTC,ETH,TON,USDT"

  -- let opts = mconcat [ [CurlFollowLocation True]
  --                    , [CurlUserAgent "Mozilla/5.0 (Android 4.4; Mobile; rv:41.0) Gecko/41.0 Firefox/41.0"]
  --                    , proxyOpt
  --                    ]

  -- body <- openLazyURIWithOpts opts req >>= either (error.show) pure
  -- L8.hPutStr stderr body

  -- let okx = decode body :: Maybe OKXMarketsData

  -- let rates =  [ x | (x :: Ins) <- universeBi okx ]

  -- let ratesDoc = hsep (fmap pretty rates)

  -- putStrLn (show ratesDoc)


