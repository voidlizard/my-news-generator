module YandexMusic where

import Control.Applicative
import Control.Monad.Except
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Functor
import Data.Generics.Uniplate.Data()
import Data.Generics.Uniplate.Operations
import Data.JsonStream.Parser
import Data.Text (Text)
import System.IO
import Safe


seekSong :: IO (Maybe Text)
seekSong = do
  s <- runExceptT $ liftIO $ LBS.hGetContents stdin <&> parseLazyByteString p

  let ss = either (const mempty) id s

  let song = headMay [ title
                     | x@("https://music.yandex.ru/home", title) :: (Text,Text) <- universeBi ss
                     ]
  pure song

  where
    p =
      "windows" .: many ( arrayOf tabs )
      where
        tabs = "tabs" .: many entries
        entries = arrayOf ( "entries" .: items )
        items = many (arrayOf ( (,) <$> "url" .: string <*> "title" .: string  )  )


