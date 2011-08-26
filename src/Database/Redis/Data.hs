{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

-- | 'Database.Redis.Data' contains functions for encoding and decoding data 
-- for communication with a Redis server. The data types supported are those 
-- described in the Redis protocol specification:
-- <http://redis.io/topics/protocol>

module Database.Redis.Data
    ( RedisData(..)
    , FromRedis
    , encode
    , fromRedis
    , commandData
    ) where
        
-- | 'RedisData' represents the data contained in a Redis request or reply. 
-- Each data constructor corresponds to one of the reply types described in 
-- the protocol specification: <http://redis.io/topics/protocol>
data RedisData = Status String 
    | Error String 
    | Integer Int 
    | Bulk (Maybe String) 
    | MultiBulk (Maybe [RedisData]) deriving (Show)
                    
-- * String encoding

-- | 'crlf' is an alias for CRLF, the line delimiter of the Redis protocol.
crlf :: String
crlf = "\r\n"

-- | 'encode' encodes a 'RedisData' as a string.
encode :: RedisData -> String
encode (MultiBulk Nothing) = "*-1\r\n"
encode (MultiBulk (Just xs)) = concat ["*", size, crlf, items]
    where 
        size = show $ length xs
        items = concat $ map encode xs 
encode x = concat $ case x of
    Status s                -> ["+", s, crlf]
    Error s                 -> ["-", s, crlf]
    Integer i           -> [":", show i, crlf]
    Bulk Nothing        -> ["$-1", crlf]
    Bulk (Just s)   -> ["$", size, crlf, s, crlf]
        where size = show $ length s
                    
-- * Commands
        
-- | 'commandData' builds a multi-bulk 'RedisData' from a list of arguments.
commandData :: [String] ->  RedisData
commandData = MultiBulk . Just . map (Bulk . Just) 

-- * Converting to Prelude types

dataTypeError :: RedisData -> IOError
dataTypeError rd = userError $ show rd

class FromRedis a where
    fromRedis :: RedisData -> Either IOError a
    
instance FromRedis String where
    fromRedis (Status s) = Right s
    fromRedis (Error s) = Right s
    fromRedis x = Left (dataTypeError x)
    
instance FromRedis (Maybe String) where
    fromRedis (Bulk m) = Right m
    fromRedis x = Left (dataTypeError x)
    
instance FromRedis (Maybe [Maybe String]) where
    fromRedis (MultiBulk m) = Right $ (fmap . fmap $ \(Bulk x) -> x) m
    fromRedis x = Left (dataTypeError x)
    
instance FromRedis Bool where
    fromRedis (Integer 0) = Right False
    fromRedis (Integer _) = Right True
    fromRedis x = Left (dataTypeError x)
    
instance FromRedis Int where
    fromRedis (Integer i) = Right i
    fromRedis x = Left (dataTypeError x)
