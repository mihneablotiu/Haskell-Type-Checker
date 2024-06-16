module Usecase.ConvertProgramToScopeGraph where

import Domain.Language.LanguageComponents
import Domain.ScopeGraph.ScopeGraph

convertProgram :: Program -> ScopeGraph
convertProgram prog =
    let (globalScope, initialGraph) = addNode (ScopeNode "Global Scope") emptyScopeGraph
        (trueNode, sg1) = addNode (DeclNode "True" TBool) initialGraph
        sg2 = addEdge globalScope trueNode D sg1
        (falseNode, sg3) = addNode (DeclNode "False" TBool) sg2
        sg4 = addEdge globalScope falseNode D sg3
        (numNode, sg5) = addNode (DeclNode "Number" TNum) sg4
        sg6 = addEdge globalScope numNode D sg5
    in foldl (convertDecl globalScope) sg6 prog

convertDecl :: Node -> ScopeGraph -> Decl -> ScopeGraph
convertDecl parentScope scopeGraph (ClassDecl tc tv funcSigs) =
    let (classNode, scopeGraphWithClassNode) = addNode (TypeClassNode tc tv) scopeGraph
        scopeGraphWithClassEdge = addEdge parentScope classNode TC scopeGraphWithClassNode
        (classScopeNode, scopeGraphWithClassScope) = addNode (ScopeNode "Class Scope") scopeGraphWithClassEdge
        scopeGraphWithParentEdge = addEdge parentScope classScopeNode P scopeGraphWithClassScope
        finalScopeGraph = addEdge classNode classScopeNode Eq scopeGraphWithParentEdge
    in foldl (convertFuncSig classScopeNode) finalScopeGraph funcSigs

convertDecl parentScope scopeGraph (InstanceDecl tc t funcDefs) =
    let (instanceNode, scopeGraphWithInstanceNode) = addNode (InstanceNode tc t) scopeGraph
        scopeGraphWithInstanceEdge = addEdge parentScope instanceNode I scopeGraphWithInstanceNode
        (instanceScopeNode, scopeGraphWithInstanceScope) = addNode (ScopeNode "Instance Scope") scopeGraphWithInstanceEdge
        scopeGraphWithParentEdge = addEdge parentScope instanceScopeNode P scopeGraphWithInstanceScope
        finalScopeGraph = addEdge instanceNode instanceScopeNode Eq scopeGraphWithParentEdge
    in foldl (convertFuncDef instanceScopeNode) finalScopeGraph funcDefs

convertDecl parentScope scopeGraph (FuncDecl name t body) =
    let (funcNode, scopeGraphWithFuncNode) = addNode (DeclNode name t) scopeGraph
        scopeGraphWithFuncEdge = addEdge parentScope funcNode D scopeGraphWithFuncNode
    in convertExpr parentScope t scopeGraphWithFuncEdge body

convertDecl parentScope scopeGraph (ValueDecl name t body) =
    let (valueNode, scopeGraphWithValueNode) = addNode (DeclNode name t) scopeGraph
        scopeGraphWithValueEdge = addEdge parentScope valueNode D scopeGraphWithValueNode
    in convertExpr parentScope t scopeGraphWithValueEdge body

convertFuncSig :: Node -> ScopeGraph -> FuncSig -> ScopeGraph
convertFuncSig parentScope scopeGraph (FuncSig name t) =
    let (funcNode, updatedScopeGraph) = addNode (DeclNode name t) scopeGraph
        finalScopeGraph = addEdge parentScope funcNode D updatedScopeGraph
    in finalScopeGraph

convertFuncDef :: Node -> ScopeGraph -> FuncDef -> ScopeGraph
convertFuncDef parentScope scopeGraph (FuncDef name t body) =
    let (funcNode, scopeGraphWithFuncNode) = addNode (DeclNode name t) scopeGraph
        scopeGraphWithFuncEdge = addEdge parentScope funcNode D scopeGraphWithFuncNode
    in convertExpr parentScope t scopeGraphWithFuncEdge body

convertExpr :: Node -> Type -> ScopeGraph -> Expr -> ScopeGraph
convertExpr parentScope _ scopeGraph (EVar name) =
    let (varNode, updatedScopeGraph) = addNode (UsageNode name) scopeGraph
        finalScopeGraph = addEdge parentScope varNode U updatedScopeGraph
    in finalScopeGraph

convertExpr parentScope _ scopeGraph (ENum n) =
    let (numNode, updatedScopeGraph) = addNode (UsageNode (show n)) scopeGraph
        finalScopeGraph = addEdge parentScope numNode U updatedScopeGraph
    in finalScopeGraph

convertExpr parentScope _ scopeGraph (EBool b) =
    let (boolNode, updatedScopeGraph) = addNode (UsageNode (show b)) scopeGraph
        finalScopeGraph = addEdge parentScope boolNode U updatedScopeGraph
    in finalScopeGraph

convertExpr parentScope _ scopeGraph (EAdd left right) =
    let leftScopeGraph = convertExpr parentScope TNum scopeGraph left
        finalScopeGraph = convertExpr parentScope TNum leftScopeGraph right
    in finalScopeGraph

convertExpr parentScope parentType scopeGraph (EApp func arg) =
    let funcScopeGraph = convertExpr parentScope parentType scopeGraph func
        finalScopeGraph = convertExpr parentScope parentType funcScopeGraph arg
    in finalScopeGraph

convertExpr parentScope parentType scopeGraph (ELam (name, _) body) =
    let (parameterType, bodyType) = case parentType of
            TFun pt bt -> (pt, bt)
            _ -> error "Parent type must be a function type"
        (lambdaScopeNode, scopeGraphWithLambdaScope) = addNode (ScopeNode (name ++ "'s Lambda Scope")) scopeGraph
        scopeGraphWithParentEdge = addEdge parentScope lambdaScopeNode P scopeGraphWithLambdaScope
        (argNode, scopeGraphWithArgNode) = addNode (DeclNode name parameterType) scopeGraphWithParentEdge
        scopeGraphWithArgEdge = addEdge lambdaScopeNode argNode D scopeGraphWithArgNode
    in convertExpr lambdaScopeNode bodyType scopeGraphWithArgEdge body
