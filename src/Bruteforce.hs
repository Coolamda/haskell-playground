module Bruteforce where

import System.Environment
import Data.Word
import Data.Char (ord)
import Data.List (find)
import Data.ByteString (pack, unpack)
import Control.Monad (replicateM)
import Text.Printf (printf)
import Crypto.Hash.SHA256 (hash)

type HexHash = String
type Password = String

main = do [hash] <- getArgs
          print . searchPasswordHash $ hash

testHash = getHash "test"

-- List aller möglichen Zeichen
characters :: String
characters = ['1'..'9'] ++ ['A'..'B'] ++ ['a'..'z']

-- Hashe alles Passwörter
allPasswordsHashed :: [(Password, HexHash)]
allPasswordsHashed = allHashes passwords

-- Generiere alle möglichen Kombination aus characters
passwords :: [Password]
passwords = concat $ combinationsFromTo 1 (length characters) characters

-- Suche Hash für Passwort in allPasswordsHashed
searchPasswordHash :: HexHash -> Maybe (Password, HexHash)
searchPasswordHash hash = searchHash hash allPasswordsHashed


-- UTILS AND HELPERS
-- Generiere eine Liste mit Strings und den dazugehörigen Hashes
allHashes :: [String] -> [(String, HexHash)]
allHashes = map (\x -> (x, getHash x))

-- Suche nach einem Hash
searchHash :: HexHash -> [(String, HexHash)] -> Maybe (String, HexHash)
searchHash hash = find ((== hash) . snd)

-- Generiere alle möglichen Kombination mit einer bestimmt Reichweite an Längen
combinationsFromTo :: Int -> Int -> [a] -> [[[a]]]
combinationsFromTo n m xs
    | n <= m = replicateM n xs : combinationsFromTo (n + 1) m xs
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
