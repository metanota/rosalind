import Data.List (maximumBy, zipWith4)
import Data.Ord  (comparing)

import Rosalind.Reader

consensus ::Char -> [String] -> [Int]
consensus c = foldl1 (zipWith (+)) . map (map (\x -> if x == c then 1 else 0))

labeled :: Char -> [a] -> [(Char, a)]
labeled c = map (\x -> (c, x))

main :: IO ()
main = do
       dnas <- readLabeledDnas "rosalind_cons.txt" >>= return . map (show . snd)
       let a = consensus 'A' dnas
       let c = consensus 'C' dnas
       let g = consensus 'G' dnas
       let t = consensus 'T' dnas
       let aa = labeled 'A' a
       let cc = labeled 'C' c
       let gg = labeled 'G' g
       let tt = labeled 'T' t
       let res = zipWith4 (\k l m n -> maximumBy (comparing snd) [k,l,m,n]) aa cc gg tt
       putStrLn $ map fst res
       putStr "A: "
       putStrLn $ unwords $ map show a
       putStr "C: "
       putStrLn $ unwords $ map show c
       putStr "G: "
       putStrLn $ unwords $ map show g
       putStr "T: "
       putStrLn $ unwords $ map show t
