module Domain.TypeCheck.SearchPattern where

data SearchPattern
    = VarUsage
    | FuncCall deriving (Eq)
