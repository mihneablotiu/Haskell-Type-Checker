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
    deriving (Show, Eq, Generic)

instance FromJSON Type

data Expr
    = EVar { varName :: String }
    | ENum { numValue :: Int }
    | EBool { boolValue :: Bool }
    | EAdd { addLeft :: Expr, addRight :: Expr }
    | EApp { appFunc :: Expr, appArg :: Expr }
    | ELam { lamArg :: String, lamBody :: Expr }
    deriving (Show, Eq, Generic)

instance FromJSON Expr

data FuncSig = FuncSig { funcSigName :: String, funcSigType :: Type } deriving (Show, Eq, Generic)
instance FromJSON FuncSig

data FuncDef = FuncDef { funcDefName :: String, funcDefBody :: Expr } deriving (Show, Eq, Generic)
instance FromJSON FuncDef

data Decl
    = ClassDecl { declTypeClass :: TypeClass, declTypeVar :: TypeVar, declFuncSigs :: [FuncSig] }
    | InstanceDecl { declTypeClass :: TypeClass, declType :: Type, declFuncDefs :: [FuncDef] }
    | FuncDecl { declFuncName :: String, declFuncType :: Type, declFuncBody :: Expr }
    deriving (Show, Eq, Generic)

instance FromJSON Decl

newtype Program = Program [Decl] deriving (Show, Eq, Generic)
instance FromJSON Program
