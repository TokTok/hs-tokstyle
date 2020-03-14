module Tokstyle.Cimple.AST
    ( AssignOp (..)
    , BinaryOp (..)
    , UnaryOp (..)
    , LiteralType (..)
    , Node (..)
    , Scope (..)
    ) where

import           Tokstyle.Cimple.Lexer (Lexeme)

data Node
    -- Preprocessor
    = PreprocInclude Lexeme
    | PreprocDefine Lexeme
    | PreprocDefineConst Lexeme Node
    | PreprocDefineMacro Lexeme [Node] Node
    | PreprocIf Node [Node] Node
    | PreprocIfdef Lexeme [Node] Node
    | PreprocIfndef Lexeme [Node] Node
    | PreprocElse [Node]
    | PreprocElif Node [Node] Node
    | PreprocError Lexeme
    | PreprocUndef Lexeme
    | PreprocDefined Lexeme
    | PreprocScopedDefine Node [Node] Node
    | MacroBodyStmt [Node] Lexeme
    | MacroBodyFunCall Node
    | MacroParam Lexeme
    -- Comments
    | Comment [Node]
    | CommentBlock Lexeme
    | CommentWord Lexeme
    -- extern "C" block
    | ExternC Lexeme Lexeme [Node] Lexeme
    -- Statements
    | CompoundStmt [Node]
    | Break
    | Goto Lexeme
    | Continue
    | Return (Maybe Node)
    | Switch Node [Node]
    | IfStmt Node [Node] (Maybe Node)
    | ForStmt (Maybe Node) (Maybe Node) (Maybe Node) [Node]
    | WhileStmt Node [Node]
    | DoWhileStmt [Node] Node
    | Case Node Node
    | Default Node
    | Label Lexeme Node
    -- Variable declarations
    | VLA Node Lexeme Node
    | VarDecl Node [Node]
    | Declarator Node (Maybe Node)
    | DeclSpecVar Lexeme
    | DeclSpecArray Node (Maybe Node)
    -- Expressions
    | InitialiserList [Node]
    | UnaryExpr UnaryOp Node
    | BinaryExpr Node BinaryOp Node
    | TernaryExpr Node Node Node
    | AssignExpr Node AssignOp Node
    | ParenExpr Node
    | CastExpr Node Node
    | SizeofExpr Node
    | LiteralExpr LiteralType Lexeme
    | VarExpr Lexeme
    | MemberAccess Node Lexeme
    | PointerAccess Node Lexeme
    | ArrayAccess Node Node
    | FunctionCall Node [Node]
    | CommentExpr Node Node
    -- Type definitions
    | EnumDecl Lexeme [Node] Lexeme
    | Enumerator Lexeme (Maybe Node)
    | Typedef Node Lexeme
    | TypedefFunction Node
    | Struct Lexeme [Node]
    | Union Lexeme [Node]
    | MemberDecl Node Node (Maybe Lexeme)
    | TyConst Node
    | TyPointer Node
    | TyStruct Lexeme
    | TyFunc Lexeme
    | TyStd Lexeme
    | TyUserDefined Lexeme
    -- Functions
    | FunctionDecl Scope Node
    | FunctionDefn Scope Node [Node]
    | FunctionPrototype Node Lexeme [Node]
    | FunctionParam Node Node
    | Ellipsis
    -- Constants
    | ConstDecl Node Lexeme
    | ConstDefn Scope Node Lexeme Node
    deriving (Show, Eq)

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
