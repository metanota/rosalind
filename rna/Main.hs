import Rosalind.Processing
import Rosalind.Reader

main :: IO ()
main = do
       dna <- readDna "rosalind_rna.txt"
       print $ dnaToRna dna
