-- contains functions for editing characters
import Data.Char

-- is this a letter to be ciphered
shouldCipher :: Char -> Bool
shouldCipher c = isLetter(c) && isAscii(c)

-- enciphers single char at a time
cipherChar :: Int -> Char -> Char
cipherChar shift c 
	| shouldCipher c = chr(ord(c) + shift)
	| otherwise = c

-- encipher whole string
cipher :: Int -> [Char] -> [Char]
cipher shift plainText = map (cipherChar shift) plainText

decipher :: Int -> [Char] -> [Char]
decipher shift plainText = cipher (-shift) plainText