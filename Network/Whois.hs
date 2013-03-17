module Network.Whois
    ( whois
    ) where

import Network
import System.IO
import Codec.Text.IConv (convert)
import Data.ByteString.Lazy.Char8 as BS (unpack,hGetContents,ByteString)
import Codec.Binary.UTF8.String (decodeString)
import Control.Applicative

convertCode :: String -> ByteString -> String
convertCode code src = decodeString $ unpack $ convert code "UTF-8" src

getWhoisServer :: String -> String
getWhoisServer _ = "192.41.192.40"

getWhoisCharCode :: String -> String
getWhoisCharCode _ = "ISO-2022-JP"

whois :: String -> IO String
whois ip = withSocketsDo $ do
    hSetBuffering stdout NoBuffering
    h <- connectTo (getWhoisServer ip) (PortNumber 43)
    hSetBuffering h LineBuffering
    hPutStrLn h ip
    convertCode (getWhoisCharCode ip) <$> BS.hGetContents h
