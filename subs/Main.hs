import qualified Data.ByteString.Char8 as C8

import Rosalind.Reader

main :: IO ()
main = do
       dnas <- readDnas "rosalind_subs.txt"
       let [dna, substr] = map C8.pack $ map show dnas
       print $ unwords $ map (show . (+1)) $ C8.findSubstrings substr dna
