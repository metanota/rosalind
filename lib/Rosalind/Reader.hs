module Rosalind.Reader
( readDna
, readRna
, readProtein
, readDnas
, readLabeledDnas
) where

import Control.Arrow (first, second)
import Data.List     (groupBy)

import Rosalind.Types

readDna :: FilePath -> IO Dna
readDna path = readFile path >>= return . Dna . head . lines

readRna :: FilePath -> IO Rna
readRna path = readFile path >>= return . Rna . head . lines

readProtein :: FilePath -> IO Protein
readProtein path = readFile path >>= return . Protein . head . lines

readDnas :: FilePath -> IO ([Dna])
readDnas path = readFile path >>= return . map Dna . lines

toPairs :: [[[a]]] -> [([a], [a])]
toPairs ns = map toPair ns
    where toPair (x:xs) = (x, concat xs)

readLabeledDnas :: FilePath -> IO ([(String, Dna)])
readLabeledDnas path = do
                       input <- readFile path
                       return $ map (second Dna) $ map (first tail) $ toPairs $ groupBy (\_ y -> '>' /= head y) $ lines input
