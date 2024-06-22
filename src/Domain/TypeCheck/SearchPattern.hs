module Domain.TypeCheck.SearchPattern where
import Domain.Language.LanguageComponents (TypeClass, Type)

data SearchPattern
    = Reference
    | InstanceUsage TypeClass Type
     deriving (Eq)
