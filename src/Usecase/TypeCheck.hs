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

runTypeCheckFuncDefs :: [FuncDef] -> ScopeGraph -> SearchState -> ([TypeError], [Path], SearchState)
runTypeCheckFuncDefs funcDefs scopeGraph searchState =
  foldl
    (\(accErrors, accPaths, accState) (FuncDef _ declaredType body) ->
      let (funcErrors, funcPaths, newState) = typeCheckExpr scopeGraph declaredType body accState
      in (accErrors ++ funcErrors, accPaths ++ funcPaths, newState)
    )
    ([], [], searchState)
    funcDefs

typeCheckDecl :: ScopeGraph -> Decl -> SearchState -> ([TypeError], [Path], SearchState)
typeCheckDecl scopeGraph (ValueDecl _ declaredType body) searchState =
  typeCheckExpr scopeGraph declaredType body searchState
typeCheckDecl scopeGraph (FuncDecl _ declaredType body) searchState =
  typeCheckExpr scopeGraph declaredType body searchState
typeCheckDecl scopeGraph (InstanceDecl _ _ funcDefs) searchState =
  runTypeCheckFuncDefs funcDefs scopeGraph searchState
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
findType _ (ENum _) searchState =
  (Right TNum, [], searchState)

findType _ (EBool _) searchState =
  (Right TBool, [], searchState)

findType scopeGraph (EVar name) searchState =
    let (varResult, newState) = checkGraphOccurrence scopeGraph name searchState Reference
    in case varResult of
        Right path -> (Right (getTypeFromNode (toNode $ last path)), [path], newState)
        Left err -> (Left err, [], newState)

findType scopeGraph (EAdd left right) searchState =
    let (leftResult, leftPaths, leftState) = findType scopeGraph left searchState
        (rightResult, rightPaths, rightState) = findType scopeGraph right leftState
    in case (leftResult, rightResult) of
        (Right TNum, Right TNum) -> (Right TNum, leftPaths ++ rightPaths, rightState)
        (Right le, Right re) -> (Left (ArithmeticError TNum le re), leftPaths ++ rightPaths, rightState)
        (Left le, _) -> (Left le, leftPaths ++ rightPaths, leftState)
        (_, Left re) -> (Left re, leftPaths ++ rightPaths, rightState)

findType scopeGraph (ELam (_, argType) body) searchState =
    let (bodyResult, bodyPaths, bodyState) = findType scopeGraph body searchState
    in case bodyResult of
        Right bType -> (Right (TFun argType bType), bodyPaths, bodyState)
        Left err -> (Left err, bodyPaths, bodyState)

findType scopeGraph (EApp func arg) searchState =
    let (funcResult, funcPaths, funcState) = findType scopeGraph func searchState
        (argResult, argPaths, argState) = findType scopeGraph arg funcState
    in case funcResult of
        Right (TFun inputType outputType) ->
          case argResult of
                Right argType ->
                    let tcEdges = filter (\(PathComponent _ et _) -> et == TC) (last funcPaths)
                        funcResolvedInClass = not (null tcEdges)
                    in if funcResolvedInClass
                       then
                        let classNode = getTypeClassFromNode $ toNode (head tcEdges)
                            initialNode = fromNode $ head $ last funcPaths
                        in case checkInstanceOccurence initialNode scopeGraph (InstanceUsage classNode argType) of
                            Right path -> (Right (substituteTypeVars outputType (extractActualTypeFromInstance (toNode (last path)))), funcPaths ++ argPaths ++ [path], argState)
                            Left _ -> (Left (InstanceNotFound (TypeClass (show classNode)) argType), funcPaths ++ argPaths, argState)
                       else if inputType == argType
                            then (Right outputType, funcPaths ++ argPaths, argState)
                            else (Left (Mismatch (show arg) inputType argType), funcPaths ++ argPaths, argState)
                Left err -> (Left err, funcPaths ++ argPaths, argState)
        Right funcType -> (Left (UnexpectedError ("Expected function type, got " ++ show funcType)), funcPaths, funcState)
        Left err -> (Left err, funcPaths, funcState)

substituteTypeVars :: Type -> Type -> Type
substituteTypeVars (TVar _) actual = actual
substituteTypeVars (TFun from to) actual = TFun (substituteTypeVars from actual) (substituteTypeVars to actual)
substituteTypeVars t _ = t

checkInstanceOccurence :: Node -> ScopeGraph -> SearchPattern -> Either TypeError Path
checkInstanceOccurence node scopeGraph searchPattern =
  case findValidPath node scopeGraph searchPattern of
    Right (t, _) -> Right t
    Left e -> Left e

checkGraphOccurrence :: ScopeGraph -> String -> SearchState -> SearchPattern -> (Either TypeError Path, SearchState)
checkGraphOccurrence scopeGraph name searchState searchPattern =
  let (graphNode, updatedCounts) = getUsageNodeFromName name scopeGraph searchState
   in case graphNode of
        Just node ->
          case findValidPath node scopeGraph searchPattern of
            Right (t, _) -> (Right t, updatedCounts)
            Left e -> (Left e, updatedCounts)
        Nothing -> undefined
