module Database.Redis.Commands.Tests
	( tests ) where
		
import           Test.Framework (Test)
import           Test.Framework.Providers.HUnit
import qualified Test.HUnit as H

import Database.Redis.Commands

tests :: [Test]
tests = [ testCase "ping" pingTest ]

pingTest :: H.Assertion
pingTest = return ()