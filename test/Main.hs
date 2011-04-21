module Main where
	
import Test.Framework (defaultMain, testGroup)

import qualified Database.Redis.Data.Tests
import qualified Database.Redis.Connection.Tests

main :: IO ()
main = defaultMain tests
	where tests = [	testGroup 	"Database.Redis.Data.Tests" 
															Database.Redis.Data.Tests.tests,
									testGroup 	"Database.Redis.Connection.Tests" 
															Database.Redis.Connection.Tests.tests]