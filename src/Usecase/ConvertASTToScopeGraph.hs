module Usecase.ConvertASTToScopeGraph where

import Domain.Language.LanguageComponents
import Domain.ScopeGraph.ScopeGraph

-- Convert a Program to a ScopeGraph
convertProgram :: Program -> ScopeGraph
convertProgram prog = 
    let (rootNode, initialGraph) = addNode ScopeNode emptyScopeGraph
    in foldl (convertDecl rootNode) initialGraph prog

-- Convert a Decl to the ScopeGraph, updating the graph accordingly
convertDecl :: Node -> ScopeGraph -> Decl -> ScopeGraph
convertDecl parentNode sg decl = case decl of
    ClassDecl tc _ funcSigs ->
        let (classNode, sg1) = addNode (TypeClassNode tc) sg
            sg2 = addEdge parentNode classNode TC sg1
        in foldl (convertFuncSig classNode) sg2 funcSigs

    InstanceDecl tc t funcDefs ->
        let (instanceNode, sg1) = addNode (InstanceNode tc t) sg
            sg2 = addEdge parentNode instanceNode I sg1
        in foldl (convertFuncDef instanceNode) sg2 funcDefs

    FuncDecl name t _ ->
        let (funcNode, sg1) = addNode (DeclNode name t) sg
            sg2 = addEdge parentNode funcNode D sg1
        in sg2

-- Convert a FuncSig to the ScopeGraph, updating the graph accordingly
convertFuncSig :: Node -> ScopeGraph -> FuncSig -> ScopeGraph
convertFuncSig parentNode sg (FuncSig name t) =
    let (sigNode, sg1) = addNode (DeclNode name t) sg
    in addEdge parentNode sigNode D sg1

-- Convert a FuncDef to the ScopeGraph, updating the graph accordingly
convertFuncDef :: Node -> ScopeGraph -> FuncDef -> ScopeGraph
convertFuncDef parentNode sg (FuncDef name _) =
    let (defNode, sg1) = addNode (DeclNode name (inferType name)) sg
    in addEdge parentNode defNode D sg1

-- Infer the type of a function definition (placeholder)
inferType :: String -> Type
inferType _ = TNum -- Placeholder, should be replaced with actual type inference
