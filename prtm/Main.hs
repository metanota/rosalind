import Rosalind.Processing
import Rosalind.Reader

main :: IO ()
main = do
       protein <- readProtein "rosalind_prtm.txt"
       print $ mass protein
