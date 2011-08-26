module Database.Redis.Commands.Tests
    ( tests ) where
        
import           Test.Framework (Test)
import           Test.Framework.Providers.HUnit
import qualified Test.HUnit as H

import Database.Redis.Connection
import Database.Redis.Commands

tests :: [Test]
tests = 
    [ 
        connectedTest "ping" $ \redis -> do
            reply <- ping redis
            H.assertEqual "reply" "PONG" reply 
            
    ,   connectedTest "echo" $ \redis -> do
            reply <- redis `echo` "hello"
            H.assertEqual "reply" (Just "hello") reply
            
    ,   connectedTest "select" $ \redis -> do
            reply <- redis `select` (testDB + 1)
            H.assertEqual "reply" "OK" reply
    
    -- ,    connectedTest "quit" $ \redis -> do
    --      reply <- quit redis
    --      H.assertEqual "reply" "OK" reply
    ]
    
testDB :: Int
testDB = 14
    
connectedTest :: String -> (Connection -> H.Assertion) -> Test
connectedTest name f = testCase name $ do
    redis <- connect localhost defaultPort
    redis `select` testDB
    f redis 