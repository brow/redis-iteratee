import Database.Redis.Connection
import Database.Redis.Commands

import Data.Maybe

main = do
	redis <- connect localhost defaultPort
	reply <- ping redis
	putStrLn reply
	reply' <- echo redis reply
	putStrLn $ fromMaybe "" reply'