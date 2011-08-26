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
            redis `auth` "password" >>= assertOK
    ,   connectedTest "echo" $ \redis -> do
            redis `echo` "hello" >>= H.assertEqual "reply" (Just "hello")
    ,   connectedTest "ping" $ \redis -> do
            ping redis >>= H.assertEqual "reply" "PONG"   
    ,   connectedTest "quit" $ \redis -> do
            quit redis  
    ,   connectedTest "select" $ \redis -> do
            redis `select` (testDB + 1) >>= assertOK

    -- Strings
    ,   connectedTest "append" $ \redis -> do
            append redis "key" "foo" >>= H.assertEqual "reply" 3
            redis `get` "key" >>= H.assertEqual "reply" (Just "foo")
            append redis "key" "bar" >>= H.assertEqual "reply" 6
            redis `get` "key" >>= H.assertEqual "reply" (Just "foobar")
    ,   connectedTest "get" $ \redis -> do
            redis `get` "key" >>= H.assertEqual "reply" Nothing
    ,   connectedTest "set" $ \redis -> do
            set redis "key" "foo" >>= assertOK
            redis `get` "key" >>= H.assertEqual "reply" (Just "foo")
            set redis "key" "bar" >>= assertOK
            redis `get` "key" >>= H.assertEqual "reply" (Just "bar")
    ]
    
testDB :: Int
testDB = 14
    
connectedTest :: String -> (Connection -> H.Assertion) -> Test
connectedTest name f = testCase name $ do
    redis <- connect localhost defaultPort
    reply <- redis `select` testDB
    if reply == "OK" 
        then do
            flushdb redis
            f redis
        else
            error "couldn't select test database"

assertOK :: String -> H.Assertion
assertOK = H.assertEqual "reply" "OK"