import Data.String.Utils (replace)

import Rosalind.Processing
import Rosalind.Reader
import Rosalind.Types

removeSubstrs :: [[Char]] -> [Char] -> [Char]
removeSubstrs []     str = str
removeSubstrs (s:ss) str = removeSubstrs ss $ replace s "" str

main :: IO ()
main = do
       dnas <- readLabeledDnas "rosalind_splc.txt"
       let origin  = show $ snd $ head dnas
       let substrs = map (show . snd) $ tail dnas
       let dna     = Dna $ removeSubstrs substrs origin
       print $ dnaToProtein dna
