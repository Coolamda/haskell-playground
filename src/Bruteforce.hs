module Bruteforce where

import Data.Word
import Data.Char (ord)
import Data.List (find)
import Data.ByteString (pack, unpack)
import Control.Monad (replicateM)
import Text.Printf (printf)
import Crypto.Hash.SHA256 (hash)

type HexHash = String
type Password = String

testHash = getHash "test"

-- List aller möglichen Zeichen
characters :: String
characters = ['1'..'9'] ++ ['A'..'B'] ++ ['a'..'z']

-- Generiere eine Liste mit Passwörtern und den dazugehörigen Hashes
allHashes :: [(Password, HexHash)]
allHashes = map (\x -> (x, getHash x)) passwords

-- Suche nach einem Hash in allHashes
searchHash :: HexHash -> Maybe (Password, HexHash)
searchHash hash = find ((== hash) . snd) allHashes

-- Generiere alle möglichen Kombination aus characters
passwords :: [Password]
passwords = concat $ combinations 1
    where combinations n
            | n <= length characters = replicateM n characters : combinations (n + 1)
            | otherwise = []

-- Generiere einen Hex Hash aus einem String
getHash :: String -> HexHash
getHash = toHex . unpack . hash . pack . toListOfWord8

-- Konvertiere einen Word8 String in Hex
toHex ::[Word8] -> String
toHex = concatMap (printf "%02x")

-- Konvertiere einen String in Word8 (Wird zum generieren von Hashes benötigt)
toListOfWord8 :: String -> [Word8]
toListOfWord8 = map (fromIntegral.ord)
