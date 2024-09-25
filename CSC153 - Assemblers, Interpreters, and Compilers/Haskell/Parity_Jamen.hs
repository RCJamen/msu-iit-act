-- Ramel Cary Jamen (2019-2093)

import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int bits = sum [w*b | (w,b) <- zip weights bits]
    where weights = iterate (*2) 1

-- bin2int = foldr (\x y -> x + 2*y) 0 - pwede ani

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod`2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

addParity :: [Bit] -> [Bit]
addParity bits
    | (sumList (make8 bits) `mod` 2 == 0) = (make8 bits) ++ [0]
    | otherwise = (make8 bits) ++ [1]

checkParity :: [Bit] -> [Bit]
checkParity bits
    | (sumList (take 8 (addParity bits)) `mod` 2 == 0) == (last bits == 0) = addParity bits
    | otherwise = error "Parity error"
