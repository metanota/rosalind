import Control.Arrow (second)
import Data.List     (genericLength, sortBy)
import Data.Ord      (comparing)

import Rosalind.Reader
import Rosalind.Types

gc :: Dna -> Double
gc (Dna dna) = gcs / len
    where len = genericLength dna
          gcs = genericLength $ filter (flip elem ['G', 'C']) dna

round' :: Double -> Int -> Double
round' f n = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

main :: IO ()
main = do
       dnas <- readLabeledDnas "rosalind_gc.txt"
       let (label, gcContent) = last $ sortBy (comparing snd) $ flip map dnas $ second gc
       putStrLn $ id label
       print $ round' (gcContent*100) 6
