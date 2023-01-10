{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ApplicativeDo #-}
module CBR where

import Control.Applicative
import Control.Monad
import Data.Aeson.Types (Value(..))
import Data.ByteString.Lazy (ByteString)
import Data.JsonStream.Parser
import Network.Curl.Download.Lazy
import Network.Curl.Opts
import System.Exit
import Data.Fixed
import Data.Data
import Prettyprinter
import Numeric

import Data.Facts.Common
import Data.Facts.Currencies

data USDRUB = USDRUB
              deriving stock (Data)

newtype instance Fact USDRUB =
  Pair_USDRUB { _usdrub :: Fixed E2 }
  deriving stock (Eq,Ord,Data,Show)

instance Pretty (Fact USDRUB) where
  pretty (Pair_USDRUB p) =
      "USDRUB" <> colon
               <+> pretty (showGFloatAlt (Just 2) (realToFrac p) "")

download :: IO ByteString
download = do
  let req = "https://www.cbr-xml-daily.ru/latest.js"
  openLazyURIWithOpts mempty req >>= either (die.show) pure

parseCBR :: Parser (Maybe (Fact USDRUB))
parseCBR = fmap Pair_USDRUB <$> rubusd
  where

rubusd :: Parser (Maybe (Fixed E2))
rubusd =  fmap realToFrac . rev . realToFrac <$> "rates" .: "USD" .: number
  where
    rev x = if x > 0 then Just (1/x) else Nothing
