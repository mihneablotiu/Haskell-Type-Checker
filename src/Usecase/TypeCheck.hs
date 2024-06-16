module Usecase.TypeCheck where
import qualified Data.Map as Map
import Domain.Language.LanguageComponents
import Domain.ScopeGraph.ScopeGraph
import Domain.TypeCheck.Path
import Domain.TypeCheck.SearchPattern (SearchPattern (VarUsage))
import Domain.TypeCheck.TypeError
import Usecase.FindPath (findValidPath)

data TypeCheckResult = TypeCheckResult
  { errors :: [TypeError],
    paths :: [Path]
  }

displayErrors :: [TypeError] -> String
displayErrors errs = unlines $ map show errs

displayPaths :: [Path] -> String
displayPaths validPaths = unlines $ map show validPaths

instance Show TypeCheckResult where
  show (TypeCheckResult errs validPaths) =
    let errorString = if null errs then "No errors found" else displayErrors errs
        pathString = if null validPaths then "No valid paths found" else displayPaths validPaths
     in errorString ++ "\n" ++ pathString

typeCheckProgram :: Program -> ScopeGraph -> TypeCheckResult
typeCheckProgram program scopeGraph =
  let initialState = Map.empty
      (determinedErrors, determinedPaths, _) = runTypeCheckDecls program scopeGraph initialState
   in TypeCheckResult determinedErrors determinedPaths

runTypeCheckDecls :: [Decl] -> ScopeGraph -> SearchState -> ([TypeError], [Path], SearchState)
runTypeCheckDecls decls scopeGraph searchState =
  foldl
    ( \(accErrors, accPaths, accState) decl ->
        let (declErrors, declPaths, newState) = typeCheckDecl scopeGraph decl accState
         in (accErrors ++ declErrors, accPaths ++ declPaths, newState)
    )
    ([], [], searchState)
    decls

typeCheckDecl :: ScopeGraph -> Decl -> SearchState -> ([TypeError], [Path], SearchState)
typeCheckDecl scopeGraph (ValueDecl _ declaredType body) searchState =
  typeCheckExpr scopeGraph declaredType body searchState
typeCheckDecl scopeGraph (FuncDecl _ declaredType body) searchState =
  typeCheckExpr scopeGraph declaredType body searchState
typeCheckDecl _ _ searchState = ([], [], searchState)

typeCheckExpr :: ScopeGraph -> Type -> Expr -> SearchState -> ([TypeError], [Path], SearchState)
typeCheckExpr scopeGraph wantedType (ENum value) searchState =
    let (numberResult, newState) = checkGraphOccurrence scopeGraph (show value) searchState VarUsage
    in case numberResult of
        Right path ->
            if getTypeFromNode (toNode $ last path) == wantedType
            then ([], [path], newState)
            else ([Mismatch (show value) TNum (getTypeFromNode (toNode $ last path))], [], newState)
        Left e -> ([e], [], newState)

typeCheckExpr scopeGraph wantedType (EBool value) searchState =
    let (boolResult, newState) = checkGraphOccurrence scopeGraph (show value) searchState VarUsage
    in case boolResult of
        Right path ->
            if getTypeFromNode (toNode $ last path) == wantedType
            then ([], [path], newState)
            else ([Mismatch (show value) TBool (getTypeFromNode (toNode $ last path))], [], newState)
        Left e -> ([e], [], newState)

typeCheckExpr scopeGraph wantedType (EVar name) searchState =
    let (varResult, newState) = checkGraphOccurrence scopeGraph name searchState VarUsage
    in case varResult of
        Right path ->
            if getTypeFromNode (toNode $ last path) == wantedType
            then ([], [path], newState)
            else ([Mismatch name wantedType (getTypeFromNode (toNode $ last path))], [], newState)
        Left e -> ([e], [], newState)

typeCheckExpr scopeGraph wantedType (EAdd left right) searchState =
    let (leftErrors, leftPaths, leftState) = typeCheckExpr scopeGraph wantedType left searchState
        (rightErrors, rightPaths, rightState) = typeCheckExpr scopeGraph wantedType right leftState
    in (leftErrors ++ rightErrors, leftPaths ++ rightPaths, rightState)

typeCheckExpr scopeGraph wantedType (ELam _ body) searchState =
    let (bodyErrors, bodyPaths, bodyState) = typeCheckExpr scopeGraph (extractOutputType wantedType) body searchState
    in (bodyErrors, bodyPaths, bodyState)

typeCheckExpr scopeGraph wantedType (EApp func arg) searchState =
    case (func, arg) of
        (EVar funcName, EVar argName) -> checkFinalStateFunctions scopeGraph wantedType funcName argName searchState
        (EVar funcName, ENum argValue) -> checkFinalStateFunctions scopeGraph wantedType funcName (show argValue) searchState
        (EVar funcName, EBool argValue) -> checkFinalStateFunctions scopeGraph wantedType funcName (show argValue) searchState
        (EVar _, EApp _ _) -> checkFinalStateFunctionButNotArgument scopeGraph wantedType func arg searchState
        _ -> ([], [], searchState)

checkFinalStateFunctionButNotArgument :: ScopeGraph -> Type -> Expr -> Expr -> SearchState -> ([TypeError], [Path], SearchState)
checkFinalStateFunctionButNotArgument scopeGraph wantedType func arg searchState =
    let (funcCheck, funcUpdatedState) = checkGraphOccurrence scopeGraph (funcName func) searchState VarUsage
    in case funcCheck of
        Right funcPath ->
            let funcType = getTypeFromNode (toNode $ last funcPath)
            in case funcType of
                TFun inputType outputType ->
                    let (argErrors, argPaths, argUpdatedState) = typeCheckExpr scopeGraph inputType arg funcUpdatedState
                    in if null argErrors
                        then if outputType == wantedType
                            then ([], funcPath : argPaths, argUpdatedState)
                            else ([Mismatch (funcName func) (TFun inputType wantedType) funcType], [], argUpdatedState)
                        else (argErrors, [], argUpdatedState)
                _ -> ([UnexpectedError $ "Expected a function type but got: " ++ show funcType], [], funcUpdatedState)
        Left funcError -> ([funcError], [], funcUpdatedState)
    where
        funcName (EVar name) = name
        funcName _ = undefined


checkFinalStateFunctions :: ScopeGraph -> Type -> [Char] -> [Char] -> SearchState -> ([TypeError], [Path], SearchState)
checkFinalStateFunctions scopeGraph wantedType funcName argName searchState =
    let (funcCheck, funcUpdatedState) = checkGraphOccurrence scopeGraph funcName searchState VarUsage
        (argCheck, argUpdatedState) = checkGraphOccurrence scopeGraph argName funcUpdatedState VarUsage
    in case (funcCheck, argCheck) of
            (Right funcPath, Right argPath) ->
                let funcType = getTypeFromNode (toNode $ last funcPath)
                    argType = getTypeFromNode (toNode $ last argPath)
                in case funcType of
                    TFun inputType outputType ->
                        if inputType == argType && outputType == wantedType
                        then ([], [funcPath, argPath], argUpdatedState)
                        else ([Mismatch funcName (TFun inputType wantedType) funcType], [], argUpdatedState)
                    _ -> ([UnexpectedError $ "Expected a function type but got: " ++ show funcType], [], argUpdatedState)
            (Left funcError, _) -> ([funcError], [], argUpdatedState)
            (_, Left argError) -> ([argError], [], argUpdatedState)

checkGraphOccurrence :: ScopeGraph -> String -> SearchState -> SearchPattern -> (Either TypeError Path, SearchState)
checkGraphOccurrence scopeGraph name searchState searchPattern =
  let (graphNode, updatedCounts) = getUsageNodeFromName name scopeGraph searchState
   in case graphNode of
        Just node ->
          case findValidPath node scopeGraph searchPattern of
            Right t -> (Right t, updatedCounts)
            Left e -> (Left e, updatedCounts)
        Nothing -> undefined
