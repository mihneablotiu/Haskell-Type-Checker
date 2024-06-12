module Usecase.TypeCheck where
import Control.Exception (TypeError)
import Data.GraphViz (Path)
import Domain.Language.LanguageComponents (Expr, Type, Decl (ValueDecl, FuncDecl), Program)
import Domain.ScopeGraph.ScopeGraph (ScopeGraph)
import Data.Either (lefts, rights)

data TypeCheckResult = TypeCheckResult {
    errors :: [TypeError],
    paths :: [Path]
}

displayPaths :: [Path] -> String
displayPaths validPaths = unlines $ map show validPaths

instance Show TypeCheckResult where
    show (TypeCheckResult errs validPahts) = "Errors:\n" ++ show errs ++ "\n\nPaths:\n" ++ displayPaths validPahts

typeCheckProgram :: Program -> ScopeGraph -> TypeCheckResult
typeCheckProgram program scopeGraph =
    let results = concatMap (typeCheckDecl scopeGraph) program
    in TypeCheckResult (lefts results) (rights results)

typeCheckDecl :: ScopeGraph -> Decl -> [Either TypeError Path]
typeCheckDecl scopeGraph (ValueDecl name declaredType body) = typeCheckExpr scopeGraph name declaredType body
typeCheckDecl scopeGraph (FuncDecl name declaredType body) = typeCheckExpr scopeGraph name declaredType body
typeCheckDecl _ _ = []


typeCheckExpr :: ScopeGraph -> String -> Type -> Expr -> [Either TypeError Path]
typeCheckExpr = undefined
