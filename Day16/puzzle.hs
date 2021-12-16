import Data.Char (digitToInt)

main = do
  file <- readFile "input.txt"
  let hex = head $ lines file
      bits = parseHex hex
      (packet, rest) = parsePacket bits
  print (eval packet)

data Packet = Packet {version :: Int, tpe :: PacketType} deriving (Show)

data PacketType = Lit Int | Operator Op [Packet] deriving (Show)

data Op = Sum | Product | Min | Max | Gr | Ls | Eq deriving (Show)

intToOp :: Int -> Op
intToOp 0 = Sum
intToOp 1 = Product
intToOp 2 = Min
intToOp 3 = Max
intToOp 5 = Gr
intToOp 6 = Ls
intToOp 7 = Eq
intToOp _ = error "Invalid operator"

eval Packet {tpe = Lit n} = n
eval Packet {tpe = Operator Sum packets} = sum $ map eval packets
eval Packet {tpe = Operator Product packets} = product $ map eval packets
eval Packet {tpe = Operator Min packets} = minimum $ map eval packets
eval Packet {tpe = Operator Max packets} = maximum $ map eval packets
eval Packet {tpe = Operator Gr (a : b : _)} = if eval a > eval b then 1 else 0
eval Packet {tpe = Operator Gr _} = error "Wrong number of subpackets for Gr"
eval Packet {tpe = Operator Ls (a : b : _)} = if eval a < eval b then 1 else 0
eval Packet {tpe = Operator Ls _} = error "Wrong number of subpackets for Ls"
eval Packet {tpe = Operator Eq (a : b : _)} = if eval a == eval b then 1 else 0
eval Packet {tpe = Operator Eq _} = error "Wrong number of subpackets for Eq"

versionSum Packet {version = version, tpe = Lit _} = version
versionSum Packet {version = version, tpe = Operator _ packets} = version + sum (map versionSum packets)

parsePacket bits =
  let consumeNum n b = (binToDec $ take n b, drop n b)
      (version, bits') = consumeNum 3 bits
      (tpe, bits'') = consumeNum 3 bits'
   in if tpe == 4
        then
          let (value, rest) = parseLiteralValue bits'' 0
           in (Packet {version = version, tpe = Lit value}, rest)
        else
          let op = intToOp tpe
              lenTypeId = head bits''
              bits''' = tail bits''
           in if lenTypeId == '1'
                then
                  let (subpacketCount, bits'''') = consumeNum 11 bits'''
                      (subpackets, rest) =
                        foldl
                          ( \(subp, bits) _ ->
                              let (nextpacket, rest) = parsePacket bits in (subp ++ [nextpacket], rest)
                          )
                          ([], bits'''')
                          [1 .. subpacketCount]
                   in (Packet {version = version, tpe = Operator op subpackets}, rest)
                else
                  let (subpacketBitsCount, bits'''') = consumeNum 15 bits'''
                      parseSubpackets bits targetLength
                        | length bits == targetLength = ([], bits)
                        | otherwise =
                          let (subpacket, rest) = parsePacket bits
                              (subpackets, rest') = parseSubpackets rest targetLength
                           in (subpacket : subpackets, rest')
                      (subpackets, rest) = parseSubpackets bits'''' (length bits'''' - subpacketBitsCount)
                   in (Packet {version = version, tpe = Operator op subpackets}, rest)

parseLiteralValue :: String -> Int -> (Int, String)
parseLiteralValue bits curr =
  let (f : val, rest) = splitAt 5 bits
      finVal = (curr * 16) + binToDec val
   in if f == '0'
        then (finVal, rest)
        else parseLiteralValue rest finVal

binToDec = foldl (\a c -> a * 2 + digitToInt c) 0

parseHex :: String -> String
parseHex = concatMap parseDigit
  where
    parseDigit '0' = "0000"
    parseDigit '1' = "0001"
    parseDigit '2' = "0010"
    parseDigit '3' = "0011"
    parseDigit '4' = "0100"
    parseDigit '5' = "0101"
    parseDigit '6' = "0110"
    parseDigit '7' = "0111"
    parseDigit '8' = "1000"
    parseDigit '9' = "1001"
    parseDigit 'A' = "1010"
    parseDigit 'B' = "1011"
    parseDigit 'C' = "1100"
    parseDigit 'D' = "1101"
    parseDigit 'E' = "1110"
    parseDigit 'F' = "1111"
    parseDigit _ = error "Incorrect hex digit"