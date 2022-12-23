import Data.ByteString.Lazy.Char8 qualified as L8
import System.Environment
import Safe
import Network.Curl.Download.Lazy
import Network.Curl.Opts

main :: IO ()
main = do
    req <- headDef (error "request not set") <$> getArgs
    let opts = [ CurlProxy "socks5://dmz:jopakita@65.108.61.217:1080"
               , CurlFollowLocation True
               ]
    body <- openLazyURIWithOpts opts req >>= either (error.show) pure
    L8.putStr body
