module Domain.TypeCheck.SearchPattern where
import Domain.Language.LanguageComponents (TypeClass)

data SearchPattern
    = ValUsage
    | FuncCall
    | InstanceUsage TypeClass
     deriving (Eq)
