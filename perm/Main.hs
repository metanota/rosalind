import Data.List (permutations)
import System.IO (readFile)

main :: IO ()
main = do
       nk <- readFile "rosalind_perm.txt"
       let (n:_) = map (\x -> read x :: Int) [nk]
       let perm = permutations [1..n]
       print $ length perm
       mapM_ print $ map (unwords . map show) $ perm
