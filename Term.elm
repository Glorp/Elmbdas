module Term where

data Term = Lam String Term | App Term Term | Var String | Define String Term | Undefine String
