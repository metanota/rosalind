import Rosalind.Reader

distance :: Eq a => [a] -> [a] -> Int
distance x y = length $ filter (==True) $ zipWith (/=) x y

main :: IO ()
main = do
       dnas <- readDnas "rosalind_hamm.txt"
       let [x, y] = map show dnas
       print $ distance x y
