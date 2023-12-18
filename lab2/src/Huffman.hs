module Huffman (compress, decompres) where

import Data.Function
import Data.Word

import qualified Data.Map        as Map
import qualified Data.List       as List
import qualified Data.ByteString as BS

type CharFrequencies = [(Char, Int)]
data Bit = Zero | One deriving (Eq, Show)
data HuffmanNode = Leaf Char Int
          | Inner HuffmanNode HuffmanNode Int
          deriving (Eq, Show, Read)

instance Ord HuffmanNode where
    compare (Leaf c1 w1) (Leaf c2 w2)
        | c1 == c2  = compare w1 w2
        | otherwise = compare c1 c2
    compare (Leaf _ w1) (Inner _ _ w2)
        | w1 == w2  = LT
        | otherwise = compare w1 w2
    compare (Inner _ _ w1) (Leaf _ w2)
        | w1 == w2  = GT
        | otherwise = compare w1 w2
    compare (Inner _ _ w1) (Inner _ _ w2)
        = compare w1 w2

wordSize :: Int
wordSize = 8

nodeWeight :: HuffmanNode -> Int
nodeWeight (Leaf _ w)    = w
nodeWeight (Inner _ _ w) = w

mergeNodes :: HuffmanNode -> HuffmanNode -> HuffmanNode
mergeNodes n1 n2 = Inner n1 n2 (nodeWeight n1 + nodeWeight n2)

charFrequencyList :: String -> CharFrequencies
charFrequencyList s = Map.toList $ updateFreqs s $ Map.fromList []
    where updateFreqs "" l     = l
          updateFreqs (c:cs) l = updateFreqs cs $ Map.insertWith (+) c 1 l

sortedFrequencies :: CharFrequencies -> CharFrequencies
sortedFrequencies = List.sortBy (compare `on` snd)

constructHuffmanTree :: CharFrequencies -> HuffmanNode
constructHuffmanTree = constructHuffmanTree' . convert . sortedFrequencies
    where constructHuffmanTree' [n] = n
          constructHuffmanTree' (n1:n2:ns)
            = constructHuffmanTree' $ List.insert (mergeNodes n1 n2) ns

          convert = foldr (\(c, w) cs -> Leaf c w : cs) []

getEncodings :: HuffmanNode -> Map.Map Char [Bit]
getEncodings t = Map.fromList $ getEncodings' t []
    where getEncodings' (Leaf c _) bits = [(c, bits)]
          getEncodings' (Inner n1 n2 _) bits
            = (getEncodings' n1 $ bits ++ [Zero]) ++ (getEncodings' n2 $ bits ++ [One])

getBits :: HuffmanNode -> String -> [Bit]
getBits t = concat . map (\c -> encodings Map.! c)
    where encodings = getEncodings t

getString :: HuffmanNode -> [Bit] -> String
getString tree bits = getString' tree tree bits ""
    where getString' _ (Leaf c _) [] s = s ++ [c]
          getString' _ _ [] s = s
          getString' r (Leaf c _) b s = getString' r r b (s ++ [c])
          getString' r (Inner left right _) (b:bs) s
            | b == Zero = getString' r left bs s
            | otherwise = getString' r right bs s

bitsToWord :: [Bit] -> Word8
bitsToWord byte = bitsToWord' byte 0
    where bitsToWord' [] acc = acc
          bitsToWord' (b:bs) acc
            | b == Zero = bitsToWord' bs (2*acc)
            | otherwise = bitsToWord' bs (2*acc + 1)

wordToBits :: Word8 -> [Bit]
wordToBits word = wordToBits' word []
    where wordToBits' 0 bits = replicate (wordSize - length bits) Zero ++ bits
          wordToBits' c bits
            | c `mod` 2 == 0 = wordToBits' (c `div` 2) (Zero:bits)
            | otherwise      = wordToBits' (c `div` 2) (One:bits)

extendToMultipleOfWordSize :: [Bit] -> [Bit]
extendToMultipleOfWordSize bits
    | (length bits) `mod` wordSize == 0 = bits
    | otherwise = bits ++ replicate (wordSize - length bits `mod` wordSize) Zero


bitstreamToByteString :: [Bit] -> BS.ByteString
bitstreamToByteString bits = bsts (extendToMultipleOfWordSize bits) []
  where bsts [] ws = BS.pack ws
        bsts b s = bsts (drop wordSize b) (s ++ [bitsToWord $ take wordSize b])

compres :: String -> IO ()
compres file = do
    text <- readFile file
    let tree = constructHuffmanTree $ charFrequencyList text
    let encoded = bitstreamToByteString $ getBits tree text
    writeFile (file ++ ".tree") (show tree)
    BS.writeFile (file ++ ".out") encoded


decompres :: String -> String -> IO ()
decompres file treeFile = do
    tree' <- readFile treeFile
    let tree = read tree'::HuffmanNode
    text <- BS.readFile file
    let decoded = getString tree $ concat $ map wordToBits $ BS.unpack text
    writeFile (file ++ ".out") decoded