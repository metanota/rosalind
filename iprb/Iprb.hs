import System.IO (readFile)

main :: IO ()
main = do
       input <- readFile "rosalind_iprb.txt"
       let [d, g, r] = map (\x -> read x :: Double) $ words input

       let s = d+g+r

       let p_d = (d/s)*((d+r+g-1)/(s-1))
       let p_g = (g/s)*((4*d+3*g+2*r-3)/(4*(s-1)))
       let p_r = (r/s)*((4*d+2*g)/(4*(s-1)))

       let p = p_d+p_g+p_r

       print p
