module LCTerm where

-- DEFINITION 1.1
data Term =
  Var String |
  Appl Term Term |
  Abst String Term deriving (Eq)

instance Show Term where
  show (Var v) = v
  show (Appl t1 (Appl t2 t3)) = show t1 ++ "(" ++ show t2 ++ show t3 ++ ")"
  show (Appl t1 t2) = show t1 ++ show t2
  show (Abst v t) = "(λ" ++ v ++ "." ++ show t ++ ")"
