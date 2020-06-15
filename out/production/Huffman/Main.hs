module Main where

import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import           Data.List
import qualified Data.Map
import           Data.Ord
import           Huffman

main :: IO ()
main = do
  firstFile <- readFile "resources/file1.txt"
  let unionFiles = lines firstFile
  let countedData = histogram (concat unionFiles)
  let sortedData = sortOn swap countedData
  print "sorted by number of unique elements"
  print sortedData
  let huffmanTree = sortedHuffman sortedData
  print "huffman tree"
  let encoding = codes huffmanTree
  print "encoded"
  let encoded = map (encode encoding) unionFiles
  mapM_ (print . showBits) encoded
  print "encoded data in 'file.bin'"
  let encBits0 = padToEight (concat encoded)
  let bits = bitpack encBits0
  Data.ByteString.Lazy.writeFile "file.bin" bits
  let Right encBits1 = bitunpack . Data.ByteString.pack . Data.ByteString.Lazy.unpack $ bits
  let decoded = map (decode huffmanTree) encoded
  print "decoded"
  print decoded
