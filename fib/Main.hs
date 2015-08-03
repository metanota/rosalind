rabbits :: Int -> [Int]
rabbits k = 1 : 1 : zipWith (\x y -> x*k + y) rabbits' (tail rabbits')
    where rabbits' = rabbits k

main :: IO ()
main = do
       nk <- readFile "rosalind_fib.txt"
       let (n:k:_) = map (\x -> read x :: Int) $ words nk
       print $ (rabbits k) !! (n-1)
