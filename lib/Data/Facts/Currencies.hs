module Data.Facts.Currencies
  ( module Data.Facts.Currencies
  , module Data.Fixed
  , IsString(..)
  ) where

import Data.Text (Text)
import Data.String (IsString(..))
import Data.Data

import Data.Fixed
import Prettyprinter

import Data.Facts.Currencies.Orphans()

data MarketData a

newtype Symbol = Symbol Text
                 deriving newtype (Eq,Ord,IsString,Read,Pretty)
                 deriving stock (Show,Data)

data family ExchangeRate prec


data family CurrencyPair a

data instance CurrencyPair Symbol = CurrencyPair Symbol Symbol
                                    deriving stock (Eq,Ord,Data,Show)

newtype instance ExchangeRate E6 = ExchRateFixed (Fixed E6)
                                   deriving stock (Eq,Ord,Data,Show)
                                   deriving newtype (Num,Real,Fractional,Read)


