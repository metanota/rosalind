module Rosalind.Types
where

data Dna     = Dna     String
data Rna     = Rna     String
data Protein = Protein String

instance Show Dna where
    show (Dna str) = id str

instance Show Rna where
    show (Rna str) = id str

instance Show Protein where
    show (Protein str) = id str
