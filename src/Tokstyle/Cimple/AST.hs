{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
module Tokstyle.Cimple.AST
    ( AssignOp (..)
    , BinaryOp (..)
    , UnaryOp (..)
    , LiteralType (..)
    , Node (..)
    , Scope (..)
    ) where

import           Tokstyle.Cimple.Lexer (Lexeme)

data Node text
    -- Preprocessor
    = PreprocInclude (Lexeme text)
    | PreprocDefine (Lexeme text)
    | PreprocDefineConst (Lexeme text) (Node text)
    | PreprocDefineMacro (Lexeme text) [Node text] (Node text)
    | PreprocIf (Node text) [Node text] (Node text)
    | PreprocIfdef (Lexeme text) [Node text] (Node text)
    | PreprocIfndef (Lexeme text) [Node text] (Node text)
    | PreprocElse [Node text]
    | PreprocElif (Node text) [Node text] (Node text)
    | PreprocError (Lexeme text)
    | PreprocUndef (Lexeme text)
    | PreprocDefined (Lexeme text)
    | PreprocScopedDefine (Node text) [Node text] (Node text)
    | MacroBodyStmt [Node text] (Lexeme text)
    | MacroBodyFunCall (Node text)
    | MacroParam (Lexeme text)
    -- Comments
    | Comment [Node text]
    | CommentBlock (Lexeme text)
    | CommentWord (Lexeme text)
    -- extern "C" block
    | ExternC (Lexeme text) (Lexeme text) [Node text] (Lexeme text)
    -- Statements
    | CompoundStmt [Node text]
    | Break
    | Goto (Lexeme text)
    | Continue
    | Return (Maybe (Node text))
    | Switch (Node text) [Node text]
    | IfStmt (Node text) [Node text] (Maybe (Node text))
    | ForStmt (Maybe (Node text)) (Maybe (Node text)) (Maybe (Node text)) [Node text]
    | WhileStmt (Node text) [Node text]
    | DoWhileStmt [Node text] (Node text)
    | Case (Node text) (Node text)
    | Default (Node text)
    | Label (Lexeme text) (Node text)
    -- Variable declarations
    | VLA (Node text) (Lexeme text) (Node text)
    | VarDecl (Node text) [Node text]
    | Declarator (Node text) (Maybe (Node text))
    | DeclSpecVar (Lexeme text)
    | DeclSpecArray (Node text) (Maybe (Node text))
    -- Expressions
    | InitialiserList [Node text]
    | UnaryExpr UnaryOp (Node text)
    | BinaryExpr (Node text) BinaryOp (Node text)
    | TernaryExpr (Node text) (Node text) (Node text)
    | AssignExpr (Node text) AssignOp (Node text)
    | ParenExpr (Node text)
    | CastExpr (Node text) (Node text)
    | SizeofExpr (Node text)
    | LiteralExpr LiteralType (Lexeme text)
    | VarExpr (Lexeme text)
    | MemberAccess (Node text) (Lexeme text)
    | PointerAccess (Node text) (Lexeme text)
    | ArrayAccess (Node text) (Node text)
    | FunctionCall (Node text) [Node text]
    | CommentExpr (Node text) (Node text)
    -- Type definitions
    | EnumDecl (Lexeme text) [Node text] (Lexeme text)
    | Enumerator (Lexeme text) (Maybe (Node text))
    | Typedef (Node text) (Lexeme text)
    | TypedefFunction (Node text)
    | Struct (Lexeme text) [Node text]
    | Union (Lexeme text) [Node text]
    | MemberDecl (Node text) (Node text) (Maybe (Lexeme text))
    | TyConst (Node text)
    | TyPointer (Node text)
    | TyStruct (Lexeme text)
    | TyFunc (Lexeme text)
    | TyStd (Lexeme text)
    | TyUserDefined (Lexeme text)
    -- Functions
    | FunctionDecl Scope (Node text)
    | FunctionDefn Scope (Node text) [Node text]
    | FunctionPrototype (Node text) (Lexeme text) [Node text]
    | FunctionParam (Node text) (Node text)
    | Ellipsis
    -- Constants
    | ConstDecl (Node text) (Lexeme text)
    | ConstDefn Scope (Node text) (Lexeme text) (Node text)
    deriving (Show, Eq, Functor, Foldable, Traversable)

data AssignOp
    = AopEq
    | AopMul
    | AopDiv
    | AopPlus
    | AopMinus
    | AopBitAnd
    | AopBitOr
    | AopBitXor
    | AopMod
    | AopLsh
    | AopRsh
    deriving (Show, Eq)

data BinaryOp
    = BopNe
    | BopEq
    | BopOr
    | BopBitXor
    | BopBitOr
    | BopAnd
    | BopBitAnd
    | BopDiv
    | BopMul
    | BopMod
    | BopPlus
    | BopMinus
    | BopLt
    | BopLe
    | BopLsh
    | BopGt
    | BopGe
    | BopRsh
    deriving (Show, Eq)

data UnaryOp
    = UopNot
    | UopNeg
    | UopMinus
    | UopAddress
    | UopDeref
    | UopIncr
    | UopDecr
    deriving (Show, Eq)

data LiteralType
    = Char
    | Int
    | Bool
    | String
    | ConstId
    deriving (Show, Eq)

data Scope
    = Global
    | Static
    deriving (Show, Eq)
