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

    -- Lists
    ,   connectedTest "empty list" $ \redis ->do
            blpop redis ["l1"] 1 >>= H.assertEqual "blpop" Nothing
            brpop redis ["l1"] 1 >>= H.assertEqual "brpop" Nothing
            rpop redis "l1" >>= H.assertEqual "rpop" Nothing
            lpop redis "l1" >>= H.assertEqual "lpop" Nothing
            lindex redis "l1" 0 >>= H.assertEqual "lindex" Nothing
            llen redis "l1" >>= H.assertEqual "llen" 0
            lrange redis "l1" 0 5 >>= H.assertEqual "lrange" (Just [])
            rpoplpush redis "l1" "l2" >>= H.assertEqual "rpoplpush" Nothing
    
    , connectedTest "construct/destruct list" $ \redis -> do
            -- construct
            rpushx redis "l1" "a" >>= H.assertEqual "rpushx empty" 0
            lpushx redis "l1" "b" >>= H.assertEqual "lpushx empty" 0
            rpush redis "l1" ["b"] >>= H.assertEqual "rpush" 1
            lpush redis "l1" ["c"] >>= H.assertEqual "lpush" 2
            rpushx redis "l1" "d" >>= H.assertEqual "rpushx" 3
            lpushx redis "l1" "e" >>= H.assertEqual "lpushx" 4
            lset redis "l1" 3 "f" >>= H.assertEqual "lset" "OK"
            mapM (lpush redis "l1" . return) ["g", "h"]

            -- inspect
            llen redis "l1" >>= H.assertEqual "llen" 6
            lindex redis "l1" 2 >>= H.assertEqual "lindex" (Just "e")
            let expected = Just $ map (Just . return) $ "hgecbf" in
                lrange redis "l1" 0 5 >>= H.assertEqual "lrange" expected

            -- destruct
            blpop redis ["l1"] 1 >>= H.assertEqual "blpop" (Just ("l1", "h"))
            brpop redis ["l1"] 1 >>= H.assertEqual "brpop" (Just ("l1", "f")) 
            lrem redis "l1" 1 "c" >>= H.assertEqual "lrem" 1
            ltrim redis "l1" 1 2 >>= H.assertEqual "ltrim" "OK"
            lpop redis "l1" >>= H.assertEqual "lpop" (Just "e")
            rpop redis "l1" >>= H.assertEqual "rpop" (Just "b")

    , connectedTest "rpoplpush" $ \redis -> do
            rpoplpush redis "l1" "l2" >>= H.assertEqual "before 1" Nothing
            brpoplpush redis "l1" "l2" 1 >>= H.assertEqual "before 2" Nothing
            mapM (rpush redis "l1" . return) ["a", "b"]
            rpoplpush redis "l1" "l2" >>= H.assertEqual "after 1" (Just "b")
            brpoplpush redis "l1" "l2" 1 >>= H.assertEqual "after 2" (Just "a")
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