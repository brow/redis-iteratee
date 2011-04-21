-- | 'Database.Redis.Commands' contains functions that mirror the set
-- of Redis commands. These functions take and return 'Prelude' data types,
-- abstracting away the need to deal with 'RedisData' directly.

module Database.Redis.Commands where
		
import Database.Redis.Connection (Connection, request)
import Database.Redis.Data (FromRedis, fromRedis, commandData)

-- * Arbitrary commands

command :: (FromRedis a) => [String] -> Connection -> IO a
command args c = do
	reply <- request c (commandData args)
	case fromRedis reply of
		Left err -> ioError err
		Right x -> return x

-- * Standard commands

-- ** Connection

echo :: Connection -> String -> IO (Maybe String)
echo c s = command ["ECHO", s] c

ping :: Connection -> IO String
ping = command ["PING"]
