import Data.List  (findIndices, nub)
import Data.Maybe (fromJust)

import Rosalind.Processing
import Rosalind.Reader
import Rosalind.Types

openReadingFrames :: Protein -> [Orf]
openReadingFrames (Protein p) = map (Orf . map fromJust)
    $ map (takeWhile (/= Nothing) . flip drop p)
    $ findIndices (== Just 'M')
    $ reverse $ dropWhile (/= Nothing) $ reverse p

allProteinsFrom :: Dna -> [Protein]
allProteinsFrom (Dna dna) = map (dnaToProtein . Dna) $ take 3 $ iterate (drop 1) dna

main :: IO ()
main = do
       dna <- readLabeledDnas "rosalind_orf.txt" >>= return . snd . head
       let orfs = nub $ concatMap openReadingFrames $ concatMap allProteinsFrom [dna, reverseComplement dna]
       mapM_ print orfs
