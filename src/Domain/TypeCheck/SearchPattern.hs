module Domain.TypeCheck.SearchPattern where

data SearchPattern
    = ValUsage
    | FuncCall deriving (Eq)
