-- | 'Database.Redis.Connection' contains IO actions for connecting to a Redis 
-- server, transmitting a command, and receiving a reply. 

module Database.Redis.Connection
	( Connection
	, connect
	, localhost
	,	defaultPort
	,	disconnect
	,	request
	) where
		
import Database.Redis.Data
import System.IO
import Data.IORef
import Network

-- | 'Connection' represents a connection to a Redis server. 
data Connection = Connection	{ handle :: Handle
															, stringRef :: IORef String
															, isOpenRef :: IORef Bool
															}

getIsOpen :: Connection -> IO Bool
getIsOpen = readIORef . isOpenRef

setIsOpen :: Connection -> Bool -> IO ()
setIsOpen = writeIORef . isOpenRef

getString :: Connection -> IO String
getString = readIORef . stringRef

setString :: Connection -> String -> IO ()
setString = writeIORef . stringRef

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
	s <- hGetContents h
	sRef <- newIORef s
	oRef <- newIORef True
	return Connection {handle=h, stringRef=sRef, isOpenRef=oRef}
		
-- | 'disconnect' closes a connection. Any subsequent requests to this
-- connection will raise an 'IOError'.
disconnect :: Connection -> IO ()
disconnect c = do
	hClose (handle c)
	c `setString` ""
	c `setIsOpen` False

-- | 'request' sends a 'RedisData' over a connection and returns the 
-- 'RedisData' it receives in reply. 
request :: Connection -> RedisData -> IO RedisData
request connection command = do
	isOpen <- getIsOpen connection
	if isOpen then do
			hPutStr (handle connection) (encode command)
			str <- getString connection
			let (rd, rest) = decode str
			connection `setString` rest
			return rd
		else do
			ioError $ userError "request on closed connection"
