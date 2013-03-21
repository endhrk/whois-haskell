module Network.Whois
    ( whois
    ) where

import Network
import System.IO
import Codec.Text.IConv (convert)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString as BS (ByteString)
import Control.Applicative

getWhoisServer :: String -> String
getWhoisServer _ = "192.41.192.40"

getWhoisCharCode :: String -> String
getWhoisCharCode _ = "ISO-2022-JP"

whois :: String -> IO BS.ByteString
whois ip = withSocketsDo $ do
    hSetBuffering stdout NoBuffering
    h <- connectTo (getWhoisServer ip) (PortNumber 43)
    hSetBuffering h LineBuffering
    hPutStrLn h ip
    c <- LBS.hGetContents h
    return $ LBS.toStrict $ convert (getWhoisCharCode ip) "UTF-8" c
