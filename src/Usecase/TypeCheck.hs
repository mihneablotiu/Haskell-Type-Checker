module Usecase.TypeCheck where

import qualified Data.Map as Map
import Domain.Language.LanguageComponents
import Domain.ScopeGraph.ScopeGraph
import Domain.TypeCheck.Path
import Domain.TypeCheck.SearchPattern
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

runTypeCheckDecls :: Program -> ScopeGraph -> SearchState -> ([TypeError], [Path], SearchState)
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
typeCheckDecl scopeGraph (InstanceDecl _ _ funcDefs) searchState =
  foldl (\(accErrors, accPaths, accState) (FuncDef _ declaredType body) ->
           let (funcErrors, funcPaths, newState) = typeCheckExpr scopeGraph declaredType body accState
            in (accErrors ++ funcErrors, accPaths ++ funcPaths, newState)
        )
        ([], [], searchState)
        funcDefs
typeCheckDecl _ _ searchState = ([], [], searchState)


typeCheckExpr :: ScopeGraph -> Type -> Expr -> SearchState -> ([TypeError], [Path], SearchState)
typeCheckExpr scopeGraph wantedType expr searchState =
    let (foundTypeResult, foundPaths, newState) = findType scopeGraph expr searchState
    in case foundTypeResult of
        Right foundType ->
            if foundType == wantedType
            then ([], foundPaths, newState)
            else ([Mismatch (show expr) wantedType foundType], foundPaths, newState)
        Left err -> ([err], foundPaths, newState)

findType :: ScopeGraph -> Expr -> SearchState -> (Either TypeError Type, [Path], SearchState)
findType scopeGraph (ENum value) searchState =
    let (numResult, newState) = checkGraphOccurrence scopeGraph (show value) searchState ValUsage
    in case numResult of
        Right path -> (Right (getTypeFromNode (toNode $ last path)), [path], newState)
        Left err -> (Left err, [], newState)

findType scopeGraph (EBool value) searchState =
    let (boolResult, newState) = checkGraphOccurrence scopeGraph (show value) searchState ValUsage
    in case boolResult of
        Right path -> (Right (getTypeFromNode (toNode $ last path)), [path], newState)
        Left err -> (Left err, [], newState)

findType scopeGraph (EVar name) searchState =
    let (varResult, newState) = checkGraphOccurrence scopeGraph name searchState FuncCall
    in case varResult of
        Right path -> (Right (getTypeFromNode (toNode $ last path)), [path], newState)
        Left err -> (Left err, [], newState)

findType scopeGraph (EAdd left right) searchState =
    let (leftResult, leftPaths, leftState) = findType scopeGraph left searchState
        (rightResult, rightPaths, rightState) = findType scopeGraph right leftState
    in case (leftResult, rightResult) of
        (Right TNum, Right TNum) -> (Right TNum, leftPaths ++ rightPaths, rightState)
        (Right _, Right _) -> (Left (UnexpectedError "Addition of non-numeric types"), leftPaths ++ rightPaths, rightState)
        (Left le, _) -> (Left le, leftPaths ++ rightPaths, leftState)
        (_, Left re) -> (Left re, leftPaths ++ rightPaths, rightState)

findType scopeGraph (ELam (_, argType) body) searchState =
    let (bodyResult, bodyPaths, bodyState) = findType scopeGraph body searchState
    in case bodyResult of
        Right bodyType -> (Right (TFun argType bodyType), bodyPaths, bodyState)
        Left err -> (Left err, bodyPaths, bodyState)

findType scopeGraph (EApp func arg) searchState =
    let (funcResult, funcPaths, funcState) = findType scopeGraph func searchState
        (argResult, argPaths, argState) = findType scopeGraph arg funcState
    in case funcResult of
        Right (TFun inputType outputType) ->
            case argResult of
                Right argType ->
                    if inputType == argType
                    then (Right outputType, funcPaths ++ argPaths, argState)
                    else (Left (Mismatch (show arg) inputType argType), funcPaths ++ argPaths, argState)
                Left err -> (Left err, funcPaths ++ argPaths, argState)
        Right funcType -> (Left (UnexpectedError ("Expected function type, got " ++ show funcType)), funcPaths, funcState)
        Left err -> (Left err, funcPaths, funcState)

checkGraphOccurrence :: ScopeGraph -> String -> SearchState -> SearchPattern -> (Either TypeError Path, SearchState)
checkGraphOccurrence scopeGraph name searchState searchPattern =
  let (graphNode, updatedCounts) = getUsageNodeFromName name scopeGraph searchState
   in case graphNode of
        Just node ->
          case findValidPath node scopeGraph searchPattern of
            Right t -> (Right t, updatedCounts)
            Left e -> (Left e, updatedCounts)
        Nothing -> undefined
