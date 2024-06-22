module Domain.TypeCheck.TypeError where
import Domain.Language.LanguageComponents
import Domain.ScopeGraph.ScopeGraph

data TypeError 
    = Mismatch { getUsage :: String,  expectedType :: Type, actualType :: Type }
    | NotInScope { getUsage :: String, getScope :: NodeInfo }
    | MultipleDeclarations { getUsage :: String, getScope :: NodeInfo }
    | UnexpectedError { message :: String }
    | InstanceNotFound { getTypeClass :: TypeClass, actualType :: Type }
    | ArithmeticError { expectedType :: Type, actualLeftType :: Type, actualRightType :: Type }

instance Show TypeError where
    show (Mismatch usage expected actual) = 
        "Type mismatch of " ++ usage ++ ": Expected " ++ show expected ++ ", but got " ++ show actual
    show (NotInScope usage scope) = "Usage of " ++ usage ++ " not in scope " ++ show scope
    show (MultipleDeclarations usage scope) = "Multiple declarations of " ++ usage ++ " in scope " ++ show scope
    show (UnexpectedError msg) = "Unexpected error: " ++ msg
    show (InstanceNotFound tc at) = "No instance found for " ++ show tc ++ " with type " ++ show at
    show (ArithmeticError et alt art) = "Arithmetic error: Expected " ++ show et ++ ", but got " ++ show alt ++ " and " ++ show art
