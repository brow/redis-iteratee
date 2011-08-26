{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Database.Redis.Data.Iteratee
    ( RedisDataException
    , redisData
    ) where

import Database.Redis.Data as Redis
import Data.Enumerator hiding (replicateM)
import Data.Enumerator.Binary.Char8 as EB hiding (replicateM)
import Data.ByteString.Char8 as BS
import Data.ByteString.Lazy.Char8 as LBS
import Data.Typeable
import Control.Monad
import Control.Exception

-- * Iteratees

-- | 'match' takes a string and consumes a number of bytes equal to the length
-- of that string. If the bytes don't match the string, an error is thrown.
match :: Monad m => LBS.ByteString -> Iteratee BS.ByteString m ()
match "" = return ()
match str = do
    first <- EB.head
    if first == Just (LBS.head str)
        then match $ LBS.tail str
        else throwError RedisDataException

-- | 'redisLine' consumes bytes from the stream until a CRLF is encountered,
-- returning the string taken. The CRLF is consumed but not included in the 
-- returned string.
redisLine :: Monad m => Iteratee BS.ByteString m LBS.ByteString
redisLine = do
    str <- EB.takeWhile (/= '\r')
    next <- EB.take 2
    if LBS.length next < 2
        then throwError RedisDataException
        else case next of 
            "\r\n" -> return str
            _ -> redisLine >>= return . (str `LBS.append` next `LBS.append`) 
        
-- | 'redisLineBytes' takes N bytes from the stream and returns them as a
-- string. It also consumes the following 2 bytes and throws an error unless
-- they are a CRLF.
redisLineBytes :: Monad m => Int -> Iteratee BS.ByteString m LBS.ByteString
redisLineBytes n = do
    str <- EB.take $ fromIntegral n
    match "\r\n"
    return str

-- | 'redisBulkData' consumes the second line of a Redis bulk reply of length 
-- N and returns it as a RedisData. If N <= 0, no bytes are consumed.
redisBulkData :: Monad m => Int -> Iteratee BS.ByteString m RedisData
redisBulkData len = if len >= 0
    then redisLineBytes len >>= return . Redis.Bulk . Just . LBS.unpack
    else return $ Redis.Bulk Nothing
    
-- | 'redisMultiBulkData' consumes the last 2*N lines of a  Redis multi-bulk 
-- reply and returns it is as RedisData. If N <= 0, no bytes are consumed.
redisMultiBulkData :: Monad m => Int -> Iteratee BS.ByteString m RedisData
redisMultiBulkData n = if n >= 0
    then replicateM n redisData >>= return . Redis.MultiBulk . Just
    else return $ Redis.MultiBulk Nothing

-- | 'redisData' consumes a complete Redis reply and returns it as a 
-- RedisData. 
redisData :: Monad m => Iteratee BS.ByteString m RedisData
redisData = do
    first <- EB.head
    rest <- redisLine
    case first of
        Just '+' -> return $ Redis.Status (LBS.unpack rest)
        Just '-' -> return $ Redis.Error (LBS.unpack rest)
        Just ':' -> return $ Redis.Integer (readLBS rest)
        Just '$' -> redisBulkData (readLBS rest)
        Just '*' -> redisMultiBulkData (readLBS rest)
        _ -> throwError RedisDataException
    where readLBS = read . LBS.unpack

-- * Exceptions

-- | 'RedisDataException' indicates missing or malformed information in a 
-- Redis reply. 
data RedisDataException = RedisDataException
    deriving (Show, Typeable)
    
instance Exception RedisDataException