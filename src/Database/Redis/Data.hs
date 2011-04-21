{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

-- | 'Database.Redis.Data' contains functions for encoding and decoding data 
-- for communication with a Redis server. The data types supported are those 
-- described in the Redis protocol specification:
-- <http://redis.io/topics/protocol>

module Database.Redis.Data
	( RedisData(..)
	, FromRedis
	, fromRedis
	, decode
	, encode
	, commandData
	) where

import Data.List (isPrefixOf)
		
-- | 'RedisData' represents the data contained in a Redis request or reply. 
-- Each data constructor corresponds to one of the reply types described in 
-- the protocol specification: <http://redis.io/topics/protocol>
data RedisData = Status String 
								| Error String 
								| Integer Int 
								| Bulk (Maybe String) 
								| MultiBulk (Maybe [RedisData]) deriving (Show)
						
-- * String encoding
								
-- | CRLF, the line delimiter of the Redis protocol.
crlf :: String
crlf = "\r\n"
		
-- | 'breakSubstring' is an analogue of 'Data.ByteString.breakSubstring' for 
-- String.
breakSubstring :: String -> String -> (String, String)
breakSubstring _ "" = ("", "")
breakSubstring s s'
	| s `isPrefixOf` s' = ("", s')
	| otherwise = ((head s'):(fst brokenTail), snd brokenTail)
	where brokenTail = breakSubstring s $ tail s'

-- | 'popLine' returns the first CRLF-delimited line of a string (sans CRLF),
-- as well as the remainder of the string.
popLine :: String -> (String, String)
popLine s = (fst broken, drop (length crlf) $ snd broken)
	where broken = breakSubstring crlf s

-- | 'decodeBulk' decodes the N-byte second line of a bulk item  
-- that follows the leading "$N".
decodeBulk :: Int -> String -> (RedisData, String)
decodeBulk (-1) s = (Bulk Nothing, s)
decodeBulk len s = (Bulk $ Just $ take len s, rest)
	where rest = drop (len + length crlf) s
	
-- | 'decodeMultiBulk' decodes the N lines of a multi-bulk item
-- that follow the leading "*N".
decodeMultiBulk :: Int -> String -> (RedisData, String)
decodeMultiBulk (-1) s = (MultiBulk Nothing, s)
decodeMultiBulk 0 s = (MultiBulk (Just []), s)
decodeMultiBulk n s = case decodeMultiBulk (n-1) rest of
		(MultiBulk (Just xs), rest)	-> (MultiBulk $ Just $ first:xs, rest)
		_ 																-> (MultiBulk Nothing, s)  
	where (first, rest) = decode s

-- | 'decode' decodes one 'RedisData' from a string, 
-- also returning the remainder of the string.
decode :: String -> (RedisData, String)
decode s = case head s of
		'+' -> (Status xs, rest)
		'-' -> (Error xs, rest)
		':' -> (Integer $ read xs, rest)
		'$' -> decodeBulk (read xs) rest
		'*' -> decodeMultiBulk (read xs) rest
	where
		(_:xs) = line
		(line, rest) = popLine s

-- | 'encode' encodes a 'RedisData' as a string.
encode :: RedisData -> String
encode (MultiBulk Nothing) = "*-1\r\n"
encode (MultiBulk (Just xs)) = concat ["*", size, crlf, items]
	where 
		size = show $ length xs
		items = concat $ map encode xs 
encode x = concat $ case x of
	Status s 				-> ["+", s, crlf]
	Error s					-> ["-", s, crlf]
	Integer i 			-> [":", show i, crlf]
	Bulk Nothing 		-> ["$-1", crlf]
	Bulk (Just s) 	-> ["$", size, crlf, s, crlf]
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
