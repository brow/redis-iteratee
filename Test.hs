{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Char8 ()
import Data.ByteString.Lazy.Char8

f :: ByteString -> Bool
f "abc" = True
f _ = False

main = do
	print $ f $ fromChunks ["abc"]
	print $ f $ fromChunks ["a","bc"]