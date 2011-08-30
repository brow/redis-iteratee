-- | 'Database.Redis.Commands' contains functions that mirror the set
-- of Redis commands. These functions take and return 'Prelude' data types,
-- abstracting away the need to deal with 'RedisData' directly.

module Database.Redis.Commands where
		
import Database.Redis.Connection (Connection, request, request')
import Database.Redis.Data (FromRedis, fromRedis, commandData)

-- * Arbitrary commands

-- | 'command' returns an action that executes a Redis command given as a list
-- of strings, where the first string is the command name and the following 
-- strings are the arguments.
command :: (FromRedis a) => Connection -> [String] -> IO a
command connection args = do
	reply <- request connection (commandData args)
	case fromRedis reply of
		Left err -> ioError err
		Right x -> return x

-- * Standard commands

-- ** Connection

auth  :: Connection -> String -> IO String
auth c s = command c ["AUTH", s]

echo :: Connection -> String -> IO (Maybe String)
echo c s = command c ["ECHO", s]

ping :: Connection -> IO String
ping c = command c ["PING"]

quit :: Connection -> IO ()
quit c = request' c $ commandData ["QUIT"]

select :: Connection -> Int -> IO String
select c i = command c ["SELECT", show i]

-- ** Lists

blpop :: Connection -> [String] -> Int -> IO (Maybe (String, String))
blpop c keys timeout = do
  reply <- command c $ ["BLPOP"] ++ keys ++ [show timeout]
  return $ case reply of
    Just [Just key, Just value] -> Just (key, value)
    _ -> Nothing

brpop :: Connection -> [String] -> Int -> IO (Maybe (String, String))
brpop c keys timeout = do
  reply <- command c $ ["BRPOP"] ++ keys ++ [show timeout]
  return $ case reply of
    Just [Just key, Just value] -> Just (key, value)
    _ -> Nothing

brpoplpush :: Connection -> String -> String -> Int -> IO (Maybe String)
brpoplpush c src dst timeout = command c ["BRPOPLPUSH", src, dst, show timeout]

lindex :: Connection -> String -> Int -> IO (Maybe String)
lindex c key index = command c ["LINDEX", key, show index]

llen :: Connection -> String -> IO Int
llen c key = command c ["LLEN", key]

lpop :: Connection -> String -> IO (Maybe String)
lpop c key = command c ["LPOP", key]

lpush :: Connection -> String -> [String] -> IO Int
lpush c key values = command c $ ["LPUSH", key] ++ values

lpushx :: Connection -> String -> String -> IO Int
lpushx c key value = command c ["LPUSHX", key, value]

lrange :: Connection -> String -> Int -> Int -> IO (Maybe [Maybe String])
lrange c key start stop = command c ["LRANGE", key, show start, show stop]

lrem :: Connection -> String -> Int -> String -> IO Int
lrem c key count value = command c ["LREM", key, show count, value]

lset :: Connection -> String -> Int -> String -> IO String
lset c key index value = command c ["LSET", key, show index, value]

ltrim :: Connection -> String -> Int -> Int -> IO String
ltrim c key start stop = command c ["LTRIM", key, show start, show stop]

rpop :: Connection -> String -> IO (Maybe String)
rpop c key = command c ["RPOP", key]

rpoplpush :: Connection -> String -> String -> IO (Maybe String)
rpoplpush c source destination = command c ["RPOPLPUSH", source, destination]

rpush :: Connection -> String -> [String] -> IO Int
rpush c key values = command c $ ["RPUSH", key] ++ values

rpushx :: Connection -> String -> String -> IO Int
rpushx c key value = command c ["RPUSHX", key, value]

-- ** Strings

append :: Connection -> String -> String -> IO Int
append c key value = command c ["APPEND", key, value]

get :: Connection -> String -> IO (Maybe String)
get c key = command c ["GET", key]

set :: Connection -> String -> String -> IO String
set c key value = command c ["SET", key, value]

-- ** Server

flushdb :: Connection -> IO String
flushdb c = command c ["FLUSHDB"]


