{-# LANGUAGE OverloadedStrings #-}

import           Database.Connection
import					 Data.List (isPrefixOf)
import           System.IO
-- import           Data.ByteString.Lazy.Char8 (ByteString)
-- import qualified Data.ByteString.Lazy.Char8 as BS
		
data RedisData = Status String 
								| Error String 
								| Integer Int 
								| Bulk (Maybe String) 
								| MultiBulk (Maybe [RedisData])
								
crlf :: String
crlf = "\r\n"

linesWith :: Eq a => [a] -> [a] -> [[a]]
linesWith _ [] = []
linesWith s s'
	| s `isPrefixOf` s' = [] : (linesWith s $ drop (length s) s')
	| null tailLines = [[head s']]
	| otherwise = ((head s'):(head tailLines)) : (tail tailLines)
	where
		tailLines = linesWith s $ tail s'
		
breakSubstring :: String -> String -> (String, String)
breakSubstring _ "" = ("", "")
breakSubstring s s'
	| s `isPrefixOf` s' = ("", s')
	| otherwise = ((head s'):(fst brokenTail), snd brokenTail)
	where
		brokenTail = breakSubstring s $ tail s'

popLine :: String -> (String, String)
popLine s = (fst broken, drop (length crlf) $ snd broken)
	where broken = breakSubstring crlf s

encode :: RedisData -> String
encode (MultiBulk Nothing) = "*-1"
encode (MultiBulk (Just xs)) = concat ["*", size, crlf, items]
	where 
		size = show $ length xs
		items = concat $ map encode xs 
encode x = concat $ case x of
	Status s -> 			["+", s, crlf]
	Error s -> 				["-", s, crlf]
	Integer i -> 			[":", show i, crlf]
	Bulk Nothing -> 	["$-1", crlf]
	Bulk (Just s) -> 	["$", size, crlf, s, crlf]
		where 
			size = show $ length s

decodeBulk :: Int -> String -> (RedisData, String)
decodeBulk (-1) s = (Bulk Nothing, s)
decodeBulk len s = (Bulk $ Just $ take len s, rest)
	where rest = drop (len + length crlf) s
	
decodeMultiBulk :: Int -> String -> (RedisData, String)
decodeMultiBulk (-1) s = (MultiBulk Nothing, s)
decodeMultiBulk 0 s = (MultiBulk (Just []), s)
decodeMultiBulk n s = case decodeMultiBulk (n-1) rest of
	(MultiBulk (Just xs), remainder) -> (MultiBulk $ Just $ first:xs, remainder)
	_ -> (MultiBulk Nothing, s)  
	where
		(first, rest) = decode s

decode :: String -> (RedisData, String)
decode s = case head s of
		'+' -> (Status xs, rest)
		'-' -> (Error xs, rest)
		':' -> (Integer $ read xs, rest)
		'$' -> decodeBulk (read xs) rest
		-- '*' -> decodeMultiBulk (read xs) rest
	where
		(_:xs) = line
		(line, rest) = popLine s
		 
main = do
	return ()
		
