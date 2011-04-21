module Database.Redis.Connection.Tests
	( tests ) where
		
import           Test.Framework (Test)
import           Test.Framework.Providers.HUnit
import qualified Test.HUnit as H

import Database.Redis.Connection

tests :: [Test]
tests = [ testCase "connect" connectTest ]

connectTest :: H.Assertion
connectTest = return ()

