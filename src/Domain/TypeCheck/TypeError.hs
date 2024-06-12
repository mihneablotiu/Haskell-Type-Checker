module Domain.TypeCheck.TypeError where

import Domain.Language.LanguageComponents
import Domain.ScopeGraph.ScopeGraph

data TypeError 
    = Mismatch { getUsage :: String,  expectedType :: Type, actualType :: Type }
    | NotInScope { getUsage :: String, getScope :: NodeInfo }
    | MultipleDeclarations { getUsage :: String, getScope :: NodeInfo }
    | UnexpectedError { message :: String }

instance Show TypeError where
    show (Mismatch usage  expected actual) = 
        "Type mismatch of " ++ show usage ++ ": Expected " ++ show expected ++ ", but got " ++ show actual
    show (NotInScope usage scope) = "Usage of " ++ show usage ++ " not in scope " ++ show scope
    show (MultipleDeclarations usage scope) = "Multiple declarations of " ++ show usage ++ " in scope " ++ show scope
    show (UnexpectedError msg) = "Unexpected error: " ++ msg
