module Usecase.ConvertASTToScopeGraph where

import Domain.Language.LanguageComponents
import Domain.ScopeGraph.ScopeGraph

-- Convert a Program to a ScopeGraph
convertProgram :: Program -> ScopeGraph
convertProgram prog =
    let (globalScope, initialGraph) = addNode (ScopeNode "Global Scope") emptyScopeGraph
    in foldl (convertDecl globalScope) initialGraph prog

convertDecl :: Node -> ScopeGraph -> Decl -> ScopeGraph
convertDecl parentScope sg (ClassDecl tc tv funcSigs) =
    let (classNode, sg') = addNode (TypeClassNode tc tv) sg
        sg'' = addEdge parentScope classNode TC sg'
        (scopeNode, sg''') = addNode (ScopeNode "Class Scope") sg''
        sg'''' = addEdge parentScope scopeNode P sg'''
        sg''''' = addEdge classNode scopeNode Eq sg''''
    in foldl (convertFuncSig scopeNode) sg''''' funcSigs

convertDecl parentScope sg (InstanceDecl tc t funcDefs) =
    let (instanceNode, sg') = addNode (InstanceNode tc t) sg
        sg'' = addEdge parentScope instanceNode I sg'
        (scopeNode, sg''') = addNode (ScopeNode "Instance Scope") sg''
        sg'''' = addEdge parentScope scopeNode P sg'''
        sg''''' = addEdge instanceNode scopeNode Eq sg''''
    in foldl (convertFuncDef scopeNode) sg''''' funcDefs

convertDecl parentScope sg (FuncDecl name t body) =
    let (funcNode, sg') = addNode (DeclNode name t) sg
        sg'' = addEdge parentScope funcNode D sg'
    in convertExpr parentScope t sg'' body

convertFuncSig :: Node -> ScopeGraph -> FuncSig -> ScopeGraph
convertFuncSig parentScope sg (FuncSig name t) =
    let (funcNode, sg') = addNode (DeclNode name t) sg
        sg'' = addEdge parentScope funcNode D sg'
    in sg''

convertFuncDef :: Node -> ScopeGraph -> FuncDef -> ScopeGraph
convertFuncDef parentScope sg (FuncDef name t body) =
    let (funcNode, sg') = addNode (DeclNode name t) sg
        sg'' = addEdge parentScope funcNode D sg'
        inputParams = extractInputParams t
        sg''' = foldl (\s (arg, ty) -> let (argNode, s') = addNode (DeclNode arg ty) s in addEdge funcNode argNode D s') sg'' inputParams
    in convertExpr parentScope t sg''' body

extractInputParams :: Type -> [(String, Type)]
extractInputParams (TFun from to) = ("arg", from) : extractInputParams to
extractInputParams _ = []

convertExpr :: Node -> Type -> ScopeGraph -> Expr -> ScopeGraph
convertExpr parentScope parentType sg (EVar name) =
    let (varNode, sg') = addNode (DeclNode name parentType) sg
        sg'' = addEdge parentScope varNode U sg'
    in sg''

convertExpr parentScope _ sg (ENum n) =
    let (numNode, sg') = addNode (DeclNode (show n) TNum) sg
        sg'' = addEdge parentScope numNode D sg'
    in sg''

convertExpr parentScope _ sg (EBool b) =
    let (boolNode, sg') = addNode (DeclNode (show b) TBool) sg
        sg'' = addEdge parentScope boolNode D sg'
    in sg''

convertExpr parentScope _ sg (EAdd left right) =
    let sg' = convertExpr parentScope TNum sg left
        sg'' = convertExpr parentScope TNum sg' right
    in sg''

convertExpr parentScope parentType sg (EApp func arg) =
    let argType = case parentType of
                    TFun _ to -> to
                    _         -> TVar (TypeVar "unknown")
        sg' = convertExpr parentScope (TFun argType parentType) sg func
        sg'' = convertExpr parentScope argType sg' arg
    in sg''

convertExpr parentScope parentType sg (ELam arg body) =
    let argType = case parentType of
                    TFun from _ -> from
                    _           -> TVar (TypeVar "unknown")
        (lamNode, sg') = addNode (DeclNode arg argType) sg
        sg'' = addEdge parentScope lamNode D sg'
        (scopeNode, sg''') = addNode (ScopeNode "Lambda Scope") sg''
        sg'''' = addEdge parentScope scopeNode P sg'''
        sg''''' = addEdge lamNode scopeNode Eq sg''''
    in convertExpr scopeNode parentType sg''''' body
