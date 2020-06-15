module Huffman where

import           Control.Concurrent
import           Control.Monad
import qualified Data.Binary.BitPut
import qualified Data.Binary.Strict.BitGet
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import           Data.Char
import           Data.List
import qualified Data.Map
import           Data.Maybe
import           Data.Ord

data HuffmanTree a
  = LeafNode a Int
  | InternalNode Int (HuffmanTree a) (HuffmanTree a)
  deriving (Eq)

frequency :: HuffmanTree a -> Int
frequency (LeafNode _ x)       = x
frequency (InternalNode x _ _) = x

-- build a huffman tree bototm-up from a list of symbols sorted by frequency
sortedHuffman :: [(a, Int)] -> HuffmanTree a
sortedHuffman
    -- first, convert each tuple into a Leaf, then combine
 = combine . map toLeaf
    -- repeatedly combine lowest frequency trees and reinsert the result into
    -- the frequency ordered list
    -- note: a priority queue could help
  where
    combine [t] = t
    combine (ta:tb:ts) = combine . insertBy (comparing frequency) (merge ta tb) $ ts
    -- make an internal node from two trees. the frequency is the sum of the
    -- two trees frequencies
    merge ta tb = InternalNode (frequency ta + frequency tb) ta tb
    -- make a Leaf from a symbol,freq tuple
    toLeaf = uncurry LeafNode

-- traverse the huffman tree generating a map from the symbol to its huffman
-- tree path (where False is left, and True is right)
codes :: Ord a => HuffmanTree a -> Data.Map.Map a [Bool]
codes = Data.Map.fromList . go []
    -- leaf nodes mark the end of a path to a symbol
  where
    go p (LeafNode s _)       = [(s, reverse p)]
    -- traverse both branches and accumulate a reverse path
    go p (InternalNode _ l r) = go (False : p) l ++ go (True : p) r

-- from a table mapping symbols to their corresponding huffman tree bit paths,
-- replace each instance of a symbol with its bit path
encode :: Ord a => Data.Map.Map a [Bool] -> [a] -> [Bool]
encode tbl = concatMap get
  where
    get x = fromJust (Data.Map.lookup x tbl)

-- from a list of bits, navigate a given huffman tree and emit its decoded
-- symbol when reaching a Leaf
decode :: HuffmanTree a -> [Bool] -> [a]
decode t0 xs0 = go t0 xs0
    -- reached leaf, emit symbol
  where
    go (LeafNode s _) bs = s : go t0 bs
    -- choose path based on bit
    go (InternalNode _ l r) (b:bs)
      | not b = go l bs
      | otherwise = go r bs
    go _ [] = []

--------------------------------------------------
-- count the number of instances each symbol occurs in a list
histogram :: Ord a => [a] -> [(a, Int)]
histogram = Data.Map.toList . foldl' insert Data.Map.empty
  where
    insert a k = Data.Map.insertWith (+) k 1 a

swap :: (a, b) -> (b, a)
swap ~(a, b) = (b, a)

showBits :: [Bool] -> String
showBits = map (intToDigit . fromEnum)

--------------------------------------------------
bitpack :: [Bool] -> Data.ByteString.Lazy.ByteString
bitpack = Data.Binary.BitPut.runBitPut . mapM_ Data.Binary.BitPut.putBit

bitunpack :: Data.ByteString.ByteString -> Either String [Bool]
bitunpack bs0 = Data.Binary.Strict.BitGet.runBitGet bs0 $ go []
  where
    go a = do
      e <- Data.Binary.Strict.BitGet.isEmpty
      if e
        then return (reverse a)
        else Data.Binary.Strict.BitGet.getBit >>= go . (: a)

--------------------------------------------------
padToEight :: [Bool] -> [Bool]
padToEight bits =
  let len = length bits
      rem = len `mod` 8
      extra = 8 - rem
      padding = replicate extra False
   in bits ++ padding
