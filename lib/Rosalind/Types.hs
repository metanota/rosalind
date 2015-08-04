module Rosalind.Types
where

import Data.Maybe (fromJust)

data Dna     = Dna     String
data Rna     = Rna     String
data Protein = Protein [Maybe Char]
data Orf     = Orf     String       deriving Eq

instance Show Dna where
    show (Dna str) = id str

instance Show Rna where
    show (Rna str) = id str

instance Show Protein where
    show (Protein chars) = map fromJust $ takeWhile (/= Nothing) chars

instance Show Orf where
    show (Orf str) = id str
