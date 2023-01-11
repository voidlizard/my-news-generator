{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
{-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE UndecidableInstances #-}


import Control.Monad
import Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.Char8 as LBS8
import Data.ByteString.Lazy (ByteString)
import Data.JsonStream.Parser hiding (Parser)
import Options.Applicative
import Data.List qualified as List
import System.Exit
import Lens.Micro.Platform
import Data.Maybe
import Data.Generics.Uniplate.Data()
import Data.Generics.Uniplate.Operations
import Data.Text.IO qualified as Text
import Safe
import Numeric

import Data.Facts.Currencies
import Coinmarketcap
import CBR qualified as CBR
import CBR (Fact(..),USDRUB)
import YandexMusic qualified as YandexMusic

import Prettyprinter

data Color = Red | Green

data Colored a = Colored Color a

newtype AsShow a = AsShow a

instance Pretty (AsShow (Fixed E2)) where
  pretty (AsShow x) = pretty $ show x

instance Pretty Color where
  pretty Red = "#c00000"
  pretty Green = "#00c000"

instance Pretty a => Pretty (Colored a) where
  pretty (Colored c x) = "%{F" <> pretty c <> "}" <> pretty x <> "%{F-}"


class HasTicker a where
  tickerSymbol  :: a -> Symbol
  tickerLow     :: a -> Bool
  tickerValue   :: Fractional b => a -> b


instance HasTicker (Fact (MarketData CMC)) where
  tickerSymbol  o = case view cmcPair o of
    CurrencyPair (Symbol a) (Symbol _) -> Symbol a

  tickerLow o = view cmcChange24h o < 0

  tickerValue o = realToFrac (realToFrac (view cmcRate o) :: Fixed E2)

newtype Report t a = Report a

instance HasTicker a => Pretty (Report t a) where
  pretty (Report o) =
    pretty (tickerSymbol o)
    <> colon
    <+> pretty (color (tickerValue o :: Double))

    where
      color | tickerLow o = Colored Red
            | otherwise   = Colored Green


main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> header "user report generator"
  <> progDesc "Generates report"
  )
  where
    parser ::  Parser (IO ())
    parser = hsubparser (  command "fxrates" (info pCoins (progDesc "coins rates report"))
                        <> command "fxrates-json" (info pFxRatesJson (progDesc "fx rates json"))
                        <> command "cbr" (info pCBR (progDesc "fxrates rouble"))
                        <> command "yandex-music-song" (info pYaMusic (progDesc "yandex music song in firefox"))
                        )

    pCoins = do
     file <- optional $ strOption (  long "file"
                                   <> short 'f'
                                   <> metavar "STRING"
                                   <> help "read from file"
                                  )
     pure $ runFxRates file

    pFxRatesJson = pure runFxRatesJson

    pCBR = pure runCBR

    pYaMusic = pure runYaMusic


runFxRates  :: Maybe String -> IO ()

runFxRates fn = do
  s <- maybe downloadMarketInfo LBS.readFile fn

  let market' = parseLazyByteString marketDataParser s
  let market = List.filter removeShit market'
  print (hsep (fmap (pretty.Report) market))

  where
    removeShit item = not ton || view cmcName item == "Toncoin"
      where
        ton = case view cmcPair item of
               CurrencyPair "TON" _ -> True
               _                    -> False

runFxRatesJson :: IO ()
runFxRatesJson = downloadMarketInfo >>= LBS.putStrLn


runCBR :: IO ()
runCBR = do
  cbr <- CBR.download <&> parseLazyByteString CBR.parseCBR
  let rate = headMay [ x | x :: Fact USDRUB <- universeBi cbr ]
  print (pretty rate)


runYaMusic :: IO ()
runYaMusic = do
  song <- YandexMusic.seekSong
  maybe exitFailure Text.putStrLn song



