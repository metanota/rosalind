import Rosalind.Processing
import Rosalind.Reader

main :: IO ()
main = do
       dna <- readDna "rosalind_revc.txt"
       print $ reverseComplement dna
