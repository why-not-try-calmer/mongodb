{- |
Module      : MongoDB TLS
Copyright   : (c)	Victor Denisov, 2016
License     : Apache 2.0
Maintainer  : Victor Denisov denisovenator@gmail.com
Stability   : alpha
Portability : POSIX

This module defines a connection interface. It could be a regular
network connection, TLS connection, a mock or anything else.
-}
module Database.MongoDB.Transport (
    Transport (..),
    fromHandle,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import System.IO
import Prelude hiding (read)

{- | Abstract transport interface

`read` should return `ByteString.null` on EOF
-}
data Transport = Transport
    { read :: Int -> IO ByteString
    , write :: ByteString -> IO ()
    , flush :: IO ()
    , close :: IO ()
    }

fromHandle :: Handle -> IO Transport
-- ^ Make connection from handle
fromHandle handle = do
    return
        Transport
            { read = ByteString.hGet handle
            , write = ByteString.hPut handle
            , flush = hFlush handle
            , close = hClose handle
            }
