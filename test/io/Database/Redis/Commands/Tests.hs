module Database.Redis.Commands.Tests
    ( tests ) where
        
import           Test.Framework (Test)
import           Test.Framework.Providers.HUnit
import qualified Test.HUnit as H

import Database.Redis.Connection
import Database.Redis.Commands

tests :: [Test]
tests = 
    -- Connection
    [   connectedTest "auth" $ \redis -> do
            redis `auth` "password" >>= H.assertEqual "reply" "OK"
    ,   connectedTest "echo" $ \redis -> do
            redis `echo` "hello" >>= H.assertEqual "reply" (Just "hello")
    ,   connectedTest "ping" $ \redis -> do
            ping redis >>= H.assertEqual "reply" "PONG"   
    ,   connectedTest "quit" $ \redis -> do
            quit redis  
    ,   connectedTest "select" $ \redis -> do
            redis `select` (testDB + 1) >>= H.assertEqual "reply" "OK"    
    ]
    
testDB :: Int
testDB = 14
    
connectedTest :: String -> (Connection -> H.Assertion) -> Test
connectedTest name f = testCase name $ do
    redis <- connect localhost defaultPort
    redis `select` testDB
    f redis 