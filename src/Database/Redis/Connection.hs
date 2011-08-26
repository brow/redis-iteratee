-- | 'Database.Redis.Connection' contains IO actions for connecting to a Redis 
-- server, transmitting a command, and receiving a reply. 

module Database.Redis.Connection
	( Connection
	, connect
	, localhost
	, defaultPort
	, disconnect
	, request
	) where
		
import Database.Redis.Data
import Database.Redis.Data.Iteratee
import Data.Enumerator
import Data.Enumerator.Binary (enumHandle)
import System.IO
import Network

-- | 'Connection' represents a connection to a Redis server. 
data Connection = Connection { handle :: Handle }

-- | 'defaultPort' is the default Redis port, 6379.
defaultPort :: PortNumber
defaultPort = 6379

-- | 'localhost' is an alias for "127.0.0.1". 
localhost :: HostName
localhost = "127.0.0.1"

-- | 'connect' creates a connection to the Redis server.
connect :: HostName -> PortNumber -> IO Connection
connect host port = withSocketsDo $ do
	h <- connectTo host (PortNumber port)
	hSetBuffering h NoBuffering
	return Connection {handle=h}
		
-- | 'disconnect' closes a connection. Any subsequent requests to this
-- connection will raise an 'IOError'.
disconnect :: Connection -> IO ()
disconnect c = do
	hClose (handle c)

-- | 'request' sends a 'RedisData' over a connection and returns the 
-- 'RedisData' it receives in reply. 
request :: Connection -> RedisData -> IO RedisData
request connection command = do
	hPutStr (handle connection) (encode command)
	reply <- run (enumHandle 1 (handle connection) $$ redisData)
	case reply of
		Right rd -> return rd
		Left _ -> ioError $ userError "iteratee exception"