module Domain.TypeChecker.TypeCheckResult where
import Domain.Language.LanguageComponents
import Domain.ScopeGraph.ScopeGraph

data TypeError 
    = Mismatch { getUsage :: String, getScope :: NodeInfo,  expectedType :: Type, actualType :: Type }
    | NotInScope { getUsage :: String, getScope :: NodeInfo }
    | MultipleDeclarations { getUsage :: String, getScope :: NodeInfo }
    | OtherError { getUsage :: String, getScope :: NodeInfo, message :: String }

instance Show TypeError where
    show (Mismatch usage scope expected actual) = 
        "Type mismatch of " ++ show usage ++ " in scope " ++ show scope ++ ": Expected " ++ show expected ++ ", but got " ++ show actual
    show (NotInScope usage scope) = "Usage of " ++ show usage ++ " not in scope " ++ show scope
    show (MultipleDeclarations usage scope) = "Multiple declarations of " ++ show usage ++ " in scope " ++ show scope
    show (OtherError usage scope msg) = "Error of " ++ show usage ++ " in scope " ++ show scope ++ ": " ++ msg


data TypeCheckResult = TypeCheckResult {
    errors :: [TypeError],
    paths :: [Path]
}

data PathComponent = PathComponent {
    fromNode :: Node,
    edgeType :: EdgeType,
    toNode :: Node
} deriving (Show, Eq)

type Path = [PathComponent]
