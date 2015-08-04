module Rosalind.Processing
( dnaToRna
, reverseComplement
, dnaToProtein
, rnaToProtein
, mass
) where

import Data.List         (dropWhileEnd)
import Data.List.Split   (chunksOf)
import Data.String.Utils (replace)

import Rosalind.Types

dnaToRna :: Dna -> Rna
dnaToRna =  Rna . replace "T" "U" . show

complement :: Char -> Char
complement 'A' = 'T'
complement 'T' = 'A'
complement 'C' = 'G'
complement 'G' = 'C'

reverseComplement :: Dna -> Dna
reverseComplement (Dna str) = Dna $ map complement $ reverse str

prot :: String -> Maybe Char
prot "UUU" = Just 'F'
prot "UUC" = Just 'F'
prot "UUA" = Just 'L'
prot "UUG" = Just 'L'
prot "UCU" = Just 'S'
prot "UCC" = Just 'S'
prot "UCA" = Just 'S'
prot "UCG" = Just 'S'
prot "UAU" = Just 'Y'
prot "UAC" = Just 'Y'
prot "UAA" = Nothing
prot "UAG" = Nothing
prot "UGU" = Just 'C'
prot "UGC" = Just 'C'
prot "UGA" = Nothing
prot "UGG" = Just 'W'
prot "CUU" = Just 'L'
prot "CUC" = Just 'L'
prot "CUA" = Just 'L'
prot "CUG" = Just 'L'
prot "CCU" = Just 'P'
prot "CCC" = Just 'P'
prot "CCA" = Just 'P'
prot "CCG" = Just 'P'
prot "CAU" = Just 'H'
prot "CAC" = Just 'H'
prot "CAA" = Just 'Q'
prot "CAG" = Just 'Q'
prot "CGU" = Just 'R'
prot "CGC" = Just 'R'
prot "CGA" = Just 'R'
prot "CGG" = Just 'R'
prot "AUU" = Just 'I'
prot "AUC" = Just 'I'
prot "AUA" = Just 'I'
prot "AUG" = Just 'M'
prot "ACU" = Just 'T'
prot "ACC" = Just 'T'
prot "ACA" = Just 'T'
prot "ACG" = Just 'T'
prot "AAU" = Just 'N'
prot "AAC" = Just 'N'
prot "AAA" = Just 'K'
prot "AAG" = Just 'K'
prot "AGU" = Just 'S'
prot "AGC" = Just 'S'
prot "AGA" = Just 'R'
prot "AGG" = Just 'R'
prot "GUU" = Just 'V'
prot "GUC" = Just 'V'
prot "GUA" = Just 'V'
prot "GUG" = Just 'V'
prot "GCU" = Just 'A'
prot "GCC" = Just 'A'
prot "GCA" = Just 'A'
prot "GCG" = Just 'A'
prot "GAU" = Just 'D'
prot "GAC" = Just 'D'
prot "GAA" = Just 'E'
prot "GAG" = Just 'E'
prot "GGU" = Just 'G'
prot "GGC" = Just 'G'
prot "GGA" = Just 'G'
prot "GGG" = Just 'G'

rnaToProtein :: Rna -> Protein
rnaToProtein (Rna rna) = Protein $ (map prot . dropWhileEnd (\x -> length x < 3) . chunksOf 3) rna

dnaToProtein :: Dna -> Protein
dnaToProtein = rnaToProtein . dnaToRna

prtm :: Char -> Double
prtm 'A' = 71.03711
prtm 'C' = 103.00919
prtm 'D' = 115.02694
prtm 'E' = 129.04259
prtm 'F' = 147.06841
prtm 'G' = 57.02146
prtm 'H' = 137.05891
prtm 'I' = 113.08406
prtm 'K' = 128.09496
prtm 'L' = 113.08406
prtm 'M' = 131.04049
prtm 'N' = 114.04293
prtm 'P' = 97.05276
prtm 'Q' = 128.05858
prtm 'R' = 156.10111
prtm 'S' = 87.03203
prtm 'T' = 101.04768
prtm 'V' = 99.06841
prtm 'W' = 186.07931
prtm 'Y' = 163.06333 

mass :: Protein -> Double
mass = sum . map prtm . show
