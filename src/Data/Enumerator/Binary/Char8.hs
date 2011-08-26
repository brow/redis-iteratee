module Data.Enumerator.Binary.Char8 
    ( module Data.Enumerator.Binary
    , Data.Enumerator.Binary.Char8.takeWhile
    , Data.Enumerator.Binary.Char8.head
    ) where

import Data.Enumerator
import Data.Enumerator.Binary hiding (takeWhile, head)
import Data.Enumerator.Binary as EB

import Data.ByteString.Char8 as BS
import Data.ByteString.Lazy.Char8 as BS.Lazy

convert :: (Enum a, Enum b) => a -> b
convert = toEnum . fromEnum

takeWhile :: Monad m   => (Char -> Bool) 
                       -> Iteratee BS.ByteString m BS.Lazy.ByteString
takeWhile f = EB.takeWhile (f . convert)

head :: Monad m => Iteratee BS.ByteString m (Maybe Char)
head = fmap (fmap convert) EB.head