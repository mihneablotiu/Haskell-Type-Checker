{-# LANGUAGE DeriveGeneric #-}

module Domain.Language.LanguageComponents where

import Data.Aeson
import GHC.Generics (Generic)

newtype TypeClass = TypeClass { getTypeClass :: String } deriving (Show, Eq, Generic)
instance FromJSON TypeClass

newtype TypeVar = TypeVar { getTypeVar :: String } deriving (Show, Eq, Generic)
instance FromJSON TypeVar

data Type
    = TNum
    | TBool
    | TVar { typeVar :: TypeVar }
    | TFun { fromType :: Type, toType :: Type }
    | TConstraint { typeClass :: TypeClass, constrainedType :: Type }
    deriving (Eq, Generic)
instance FromJSON Type

instance Show Type where
    show TNum = "Int"
    show TBool = "Bool"
    show (TVar (TypeVar tv)) = tv
    show (TFun from to) = "(" ++ show from ++ " -> " ++ show to ++ ")"
    show (TConstraint (TypeClass tc) t) = tc ++ " " ++ show t

data Expr
    = EVar { varName :: String }
    | ENum { numValue :: Int }
    | EBool { boolValue :: Bool }
    | EAdd { addLeft :: Expr, addRight :: Expr }
    | EApp { appFunc :: Expr, appArg :: Expr }
    | ELam { lamArg :: (String, Type), lamBody :: Expr }
    deriving (Eq, Generic)
instance FromJSON Expr

instance Show Expr where
    show (EVar name) = name
    show (ENum value) = show value
    show (EBool value) = show value
    show (EAdd left right) = show left ++ " + " ++ show right
    show (EApp func arg) = show func ++ " " ++ show arg
    show (ELam (argName, argType) body) = "\\" ++ argName ++ " : " ++ show argType ++ " -> " ++ show body

data FuncSig = FuncSig { funcSigName :: String, funcSigType :: Type } deriving (Show, Eq, Generic)
instance FromJSON FuncSig

data FuncDef = FuncDef { funcDefName :: String, funcDefType :: Type, funcDefBody :: Expr } deriving (Show, Eq, Generic)
instance FromJSON FuncDef

data Decl
    = ClassDecl { declTypeClass :: TypeClass, declTypeVar :: TypeVar, declFuncSigs :: [FuncSig] }
    | InstanceDecl { declTypeClass :: TypeClass, declType :: Type, declFuncDefs :: [FuncDef] }
    | FuncDecl { declFuncName :: String, declFuncType :: Type, declFuncBody :: Expr }
    | ValueDecl { declValueName :: String, declValueType :: Type, declValueBody :: Expr }
    deriving (Show, Eq, Generic)
instance FromJSON Decl

type Program = [Decl]

extractInputType :: Type -> Type
extractInputType (TFun from _) = from
extractInputType _ = error "Type is not a function"

extractOutputType :: Type -> Type
extractOutputType (TFun _ to) = to
extractOutputType _ = error "Type is not a function"
