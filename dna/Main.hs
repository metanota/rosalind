import qualified Data.Map as M (fromListWith, toList)

import Rosalind.Reader

main :: IO ()
main = do
       dna <- readDna "rosalind_dna.txt"
       let l = M.toList $ M.fromListWith (+) [(c, 1) | c <- (show dna)]
       print $ unwords $ map (show . snd) l
