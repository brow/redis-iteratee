module Database.Redis.Data.Tests
    ( tests ) where
        
import           Test.Framework (Test)
import           Test.Framework.Providers.HUnit
import qualified Test.HUnit as H

import Database.Redis.Data as Data

tests :: [Test]
tests = [ testCase "encode" encodeTest ]

encodeTest :: H.Assertion
encodeTest = do
    H.assertEqual "Status"              "+OK\r\n"       (encode $ Data.Status "OK") 
    H.assertEqual "Error"               "-ERROR\r\n"    (encode $ Data.Error "ERROR") 
    H.assertEqual "Integer"             ":0\r\n"        (encode $ Data.Integer 0) 
    H.assertEqual "Bulk Nothing"        "$-1\r\n"       (encode $ Data.Bulk Nothing) 
    H.assertEqual "Bulk \"\""           "$0\r\n\r\n"    (encode $ Data.Bulk $ Just "") 
    H.assertEqual "MultiBulk Nothing"   "*-1\r\n"       (encode $ Data.MultiBulk Nothing) 
    H.assertEqual "MultiBulk"       
        (encode $ Data.MultiBulk $ Just [Bulk (Just "hello"), Bulk Nothing]) 
        "*2\r\n$5\r\nhello\r\n$-1\r\n"

