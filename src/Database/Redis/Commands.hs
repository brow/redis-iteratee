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

auth :: Connection -> String -> IO String
auth c s = command c ["AUTH", s]

echo :: Connection -> String -> IO (Maybe String)
echo c s = command c ["ECHO", s]

ping :: Connection -> IO String
ping c = command c ["PING"]

quit :: Connection -> IO ()
quit c = request' c $ commandData ["QUIT"]

select :: Connection -> Int -> IO String
select c i = command c ["SELECT", show i]

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


