import Rosalind.Processing
import Rosalind.Reader

main :: IO ()
main = do
       rna <- readRna "rosalind_prot.txt"
       print $ rnaToProtein rna
