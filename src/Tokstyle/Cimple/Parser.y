{
module Tokstyle.Cimple.Parser where

import           Tokstyle.Cimple.AST    (AssignOp (..), BinaryOp (..),
                                         LiteralType (..), Node (..),
                                         Scope (..), UnaryOp (..))
import           Tokstyle.Cimple.Lexer  (Alex, AlexPosn, Lexeme (..),
                                         alexMonadScan)
import           Tokstyle.Cimple.Tokens (LexemeClass (..))
}

-- Conflict between (static) FunctionDecl and (static) ConstDecl.
%expect 2

%name parseCimple
%error {parseError}
%lexer {lexwrap} {L _ Eof _}
%monad {Alex}
%tokentype {Lexeme}
%token
    ID_CONST			{ L _ IdConst			_ }
    ID_FUNC_TYPE		{ L _ IdFuncType		_ }
    ID_STD_TYPE			{ L _ IdStdType			_ }
    ID_SUE_TYPE			{ L _ IdSueType			_ }
    ID_VAR			{ L _ IdVar			_ }
    break			{ L _ KwBreak			_ }
    case			{ L _ KwCase			_ }
    const			{ L _ KwConst			_ }
    continue			{ L _ KwContinue		_ }
    default			{ L _ KwDefault			_ }
    do				{ L _ KwDo			_ }
    else			{ L _ KwElse			_ }
    enum			{ L _ KwEnum			_ }
    extern			{ L _ KwExtern			_ }
    for				{ L _ KwFor			_ }
    goto			{ L _ KwGoto			_ }
    if				{ L _ KwIf			_ }
    return			{ L _ KwReturn			_ }
    sizeof			{ L _ KwSizeof			_ }
    static			{ L _ KwStatic			_ }
    struct			{ L _ KwStruct			_ }
    switch			{ L _ KwSwitch			_ }
    typedef			{ L _ KwTypedef			_ }
    union			{ L _ KwUnion			_ }
    VLA				{ L _ KwVla			_ }
    void			{ L _ KwVoid			_ }
    while			{ L _ KwWhile			_ }
    LIT_CHAR			{ L _ LitChar			_ }
    LIT_FALSE			{ L _ LitFalse			_ }
    LIT_TRUE			{ L _ LitTrue			_ }
    LIT_INTEGER			{ L _ LitInteger		_ }
    LIT_STRING			{ L _ LitString			_ }
    LIT_SYS_INCLUDE		{ L _ LitSysInclude		_ }
    '&'				{ L _ PctAmpersand		_ }
    '&&'			{ L _ PctAmpersandAmpersand	_ }
    '&='			{ L _ PctAmpersandEq		_ }
    '->'			{ L _ PctArrow			_ }
    '*'				{ L _ PctAsterisk		_ }
    '*='			{ L _ PctAsteriskEq		_ }
    '^'				{ L _ PctCaret			_ }
    '^='			{ L _ PctCaretEq		_ }
    ':'				{ L _ PctColon			_ }
    ','				{ L _ PctComma			_ }
    '!'				{ L _ PctEMark			_ }
    '!='			{ L _ PctEMarkEq		_ }
    '='				{ L _ PctEq			_ }
    '=='			{ L _ PctEqEq			_ }
    '>'				{ L _ PctGreater		_ }
    '>='			{ L _ PctGreaterEq		_ }
    '>>'			{ L _ PctGreaterGreater		_ }
    '>>='			{ L _ PctGreaterGreaterEq	_ }
    '{'				{ L _ PctLBrace			_ }
    '['				{ L _ PctLBrack			_ }
    '<'				{ L _ PctLess			_ }
    '<='			{ L _ PctLessEq			_ }
    '<<'			{ L _ PctLessLess		_ }
    '<<='			{ L _ PctLessLessEq		_ }
    '('				{ L _ PctLParen			_ }
    '-'				{ L _ PctMinus			_ }
    '-='			{ L _ PctMinusEq		_ }
    '--'			{ L _ PctMinusMinus		_ }
    '%'				{ L _ PctPercent		_ }
    '%='			{ L _ PctPercentEq		_ }
    '.'				{ L _ PctPeriod			_ }
    '...'			{ L _ PctEllipsis		_ }
    '|'				{ L _ PctPipe			_ }
    '|='			{ L _ PctPipeEq			_ }
    '||'			{ L _ PctPipePipe		_ }
    '+'				{ L _ PctPlus			_ }
    '+='			{ L _ PctPlusEq			_ }
    '++'			{ L _ PctPlusPlus		_ }
    '?'				{ L _ PctQMark			_ }
    '}'				{ L _ PctRBrace			_ }
    ']'				{ L _ PctRBrack			_ }
    ')'				{ L _ PctRParen			_ }
    ';'				{ L _ PctSemicolon		_ }
    '/'				{ L _ PctSlash			_ }
    '/='			{ L _ PctSlashEq		_ }
    '~'				{ L _ PctTilde			_ }
    'defined'			{ L _ PpDefined			_ }
    '#define'			{ L _ PpDefine			_ }
    '#elif'			{ L _ PpElif			_ }
    '#else'			{ L _ PpElse			_ }
    '#endif'			{ L _ PpEndif			_ }
    '#error'			{ L _ PpError			_ }
    '#if'			{ L _ PpIf			_ }
    '#ifdef'			{ L _ PpIfdef			_ }
    '#ifndef'			{ L _ PpIfndef			_ }
    '#include'			{ L _ PpInclude			_ }
    '#undef'			{ L _ PpUndef			_ }
    '\n'			{ L _ PpNewline			_ }
    '/**/'			{ L _ CmtBlock			_ }
    '/*'			{ L _ CmtStart			_ }
    '*/'			{ L _ CmtEnd			_ }
    'Copyright'			{ L _ CmtSpdxCopyright		_ }
    'License'			{ L _ CmtSpdxLicense		_ }
    COMMENT_CODE		{ L _ CmtCode			_ }
    COMMENT_WORD		{ L _ CmtWord			_ }

%left ','
%right '=' '+=' '-=' '*=' '/=' '%=' '<<=' '>>=' '&=' '^=' '|='
%right '?' ':'
%left '||'
%left '&&'
%left '|'
%left '^'
%left '&'
%left '!=' '=='
%left '<' '<=' '>' '>='
%left '<<' '>>'
%left '+' '-'
%left '*' '/' '%'
%right CAST ADDRESS NEG DEREF sizeof '!' '~' '++' '--'
%left '->' '.' '(' '['

%%

TranslationUnit :: { [Node] }
TranslationUnit
:	ToplevelDecls							{ reverse $1 }

ToplevelDecls :: { [Node] }
ToplevelDecls
:	ToplevelDecl							{ [$1] }
|	ToplevelDecls ToplevelDecl					{ $2 : $1 }

ToplevelDecl :: { Node }
ToplevelDecl
:	PreprocIfdef(ToplevelDecls)					{ $1 }
|	PreprocIf(ToplevelDecls)					{ $1 }
|	PreprocInclude							{ $1 }
|	PreprocDefine							{ $1 }
|	PreprocUndef							{ $1 }
|	PreprocError							{ $1 }
|	ExternC								{ $1 }
|	TypedefDecl							{ $1 }
|	AggregateDecl							{ $1 }
|	EnumDecl							{ $1 }
|	FunctionDecl							{ $1 }
|	ConstDecl							{ $1 }
|	Comment								{ $1 }

Comment :: { Node }
Comment
:	'/*' CommentBody '*/'						{ Comment (reverse $2) }
|	'/**/'								{ CommentBlock $1 }

CommentBody :: { [Node] }
CommentBody
:	CommentWord							{ [$1] }
|	CommentBody CommentWord						{ $2 : $1 }

CommentWord :: { Node }
CommentWord
:	COMMENT_WORD							{ CommentWord $1 }
|	COMMENT_CODE							{ CommentWord $1 }
|	LIT_INTEGER							{ CommentWord $1 }
|	LIT_STRING							{ CommentWord $1 }
|	'Copyright'							{ CommentWord $1 }
|	'License'							{ CommentWord $1 }
|	'.'								{ CommentWord $1 }
|	'?'								{ CommentWord $1 }
|	'!'								{ CommentWord $1 }
|	','								{ CommentWord $1 }
|	';'								{ CommentWord $1 }
|	':'								{ CommentWord $1 }
|	'('								{ CommentWord $1 }
|	')'								{ CommentWord $1 }
|	'<'								{ CommentWord $1 }
|	'>'								{ CommentWord $1 }
|	'/'								{ CommentWord $1 }
|	'+'								{ CommentWord $1 }
|	'-'								{ CommentWord $1 }
|	'='								{ CommentWord $1 }
|	'\n'								{ CommentWord $1 }

PreprocIfdef(decls)
:	'#ifdef' ID_CONST decls PreprocElse(decls) '#endif'		{ PreprocIfdef $2 $3 $4 }
|	'#ifndef' ID_CONST decls PreprocElse(decls) '#endif'		{ PreprocIfndef $2 $3 $4 }

PreprocIf(decls)
:	'#if' ConstExpr '\n' decls PreprocElse(decls) '#endif'		{ PreprocIf $2 $4 $5 }

PreprocElse(decls)
:									{ PreprocElse [] }
|	'#else' decls							{ PreprocElse $2 }
|	'#elif' ConstExpr '\n' decls PreprocElse(decls)			{ PreprocElif $2 $4 $5 }

PreprocError :: { Node }
PreprocError
:	'#error' LIT_STRING						{ PreprocError $2 }

PreprocInclude :: { Node }
PreprocInclude
:	'#include' LIT_STRING						{ PreprocInclude $2 }
|	'#include' LIT_SYS_INCLUDE					{ PreprocInclude $2 }

PreprocDefine :: { Node }
PreprocDefine
:	'#define' ID_CONST '\n'						{ PreprocDefine $2 }
|	'#define' ID_CONST ConstExpr '\n'				{ PreprocDefineConst $2 $3 }
|	'#define' ID_CONST MacroParamList MacroBody '\n'		{ PreprocDefineMacro $2 $3 $4 }

PreprocUndef :: { Node }
PreprocUndef
:	'#undef' ID_CONST						{ PreprocUndef $2 }

ConstExpr :: { Node }
ConstExpr
:	LiteralExpr							{ $1 }
|	'defined' '(' ID_CONST ')'					{ PreprocDefined $3 }
|	PureExpr(ConstExpr)						{ $1 }

MacroParamList :: { [Node] }
MacroParamList
:	'(' ')'								{ [] }
|	'(' MacroParams ')'						{ reverse $2 }
|	'(' MacroParams ',' '...' ')'					{ reverse $ Ellipsis : $2 }

MacroParams :: { [Node] }
MacroParams
:	MacroParam							{ [$1] }
|	MacroParams ',' MacroParam					{ $3 : $1 }

MacroParam :: { Node }
MacroParam
:	ID_VAR								{ MacroParam $1 }

MacroBody :: { Node }
MacroBody
:	do CompoundStmt while '(' LIT_INTEGER ')'			{ MacroBodyStmt $2 $5 }
|	FunctionCall							{ MacroBodyFunCall $1 }

ExternC :: { Node }
ExternC
:	'#ifdef' ID_CONST
	extern LIT_STRING '{'
	'#endif'
	ToplevelDecls
	'#ifdef' ID_CONST
	'}'
	'#endif'							{ ExternC $2 $4 $7 $9 }

Stmts :: { [Node] }
Stmts
:	Stmt								{ [$1] }
|	Stmts Stmt							{ $2 : $1 }

Stmt :: { Node }
Stmt
:	PreprocIfdef(Stmts)						{ $1 }
|	PreprocIf(Stmts)						{ $1 }
|	PreprocDefine Stmts PreprocUndef				{ PreprocScopedDefine $1 $2 $3 }
|	LabelStmt							{ $1 }
|	DeclStmt							{ $1 }
|	CompoundStmt							{ CompoundStmt $1 }
|	IfStmt								{ $1 }
|	ForStmt								{ $1 }
|	WhileStmt							{ $1 }
|	DoWhileStmt							{ $1 }
|	AssignExpr ';'							{ $1 }
|	ExprStmt ';'							{ $1 }
|	FunctionCall ';'						{ $1 }
|	break ';'							{ Break }
|	goto ID_CONST ';'						{ Goto $2 }
|	continue ';'							{ Continue }
|	return ';'							{ Return Nothing }
|	return Expr ';'							{ Return (Just $2) }
|	switch '(' Expr ')' CompoundStmt				{ Switch $3 $5 }
|	Comment								{ $1 }

IfStmt :: { Node }
IfStmt
:	if '(' Expr ')' CompoundStmt					{ IfStmt $3 $5 Nothing }
|	if '(' Expr ')' CompoundStmt else IfStmt			{ IfStmt $3 $5 (Just $7) }
|	if '(' Expr ')' CompoundStmt else CompoundStmt			{ IfStmt $3 $5 (Just (CompoundStmt $7)) }

ForStmt :: { Node }
ForStmt
:	for '(' ForInit Opt(Expr) ';' Opt(ForNext) ')' CompoundStmt	{ ForStmt $3 $4 $6 $8 }

ForInit :: { Maybe Node }
ForInit
:	';'								{ Nothing }
|	AssignExpr ';'							{ Just $1 }
|	SingleVarDecl							{ Just $1 }

ForNext :: { Node }
ForNext
:	ExprStmt							{ $1 }
|	AssignExpr							{ $1 }

Opt(x)
:									{ Nothing }
|	x								{ Just $1 }

WhileStmt :: { Node }
WhileStmt
:	while '(' Expr ')' CompoundStmt					{ WhileStmt $3 $5 }

DoWhileStmt :: { Node }
DoWhileStmt
:	do CompoundStmt while '(' Expr ')' ';'				{ DoWhileStmt $2 $5 }

LabelStmt :: { Node }
LabelStmt
:	case Expr ':' Stmt						{ Case $2 $4 }
|	default ':' Stmt						{ Default $3 }
|	ID_CONST ':' Stmt						{ Label $1 $3 }

DeclStmt :: { Node }
DeclStmt
:	VarDecl								{ $1 }
|	VLA '(' Type ',' ID_VAR ',' Expr ')' ';'			{ VLA $3 $5 $7 }

SingleVarDecl :: { Node }
SingleVarDecl
:	QualType Declarator ';'						{ VarDecl $1 [$2] }

VarDecl :: { Node }
VarDecl
:	QualType Declarators ';'					{ VarDecl $1 (reverse $2) }

Declarators :: { [Node] }
Declarators
:	Declarator							{ [$1] }
|	Declarators ',' Declarator					{ $3 : $1 }

Declarator :: { Node }
Declarator
:	DeclSpec(Expr) '=' InitialiserExpr				{ Declarator $1 (Just $3) }
|	DeclSpec(Expr)							{ Declarator $1 Nothing }

InitialiserExpr :: { Node }
InitialiserExpr
:	InitialiserList							{ InitialiserList $1 }
|	Expr								{ $1 }

DeclSpec(expr)
:	ID_VAR								{ DeclSpecVar $1 }
|	DeclSpec(expr) '[' ']'						{ DeclSpecArray $1 Nothing }
|	DeclSpec(expr) '[' expr ']'					{ DeclSpecArray $1 (Just $3) }

InitialiserList :: { [Node] }
InitialiserList
:	'{' Initialisers '}'						{ reverse $2 }
|	'{' Initialisers ',' '}'					{ reverse $2 }

Initialisers :: { [Node] }
Initialisers
:	Initialiser							{ [$1] }
|	Initialisers ',' Initialiser					{ $3 : $1 }

Initialiser :: { Node }
Initialiser
:	Expr								{ $1 }
|	InitialiserList							{ InitialiserList $1 }

CompoundStmt :: { [Node] }
CompoundStmt
:	'{' Stmts '}'							{ $2 }

PureExpr(x)
:	x '!=' x							{ BinaryExpr $1 BopNe $3 }
|	x '==' x							{ BinaryExpr $1 BopEq $3 }
|	x '||' x							{ BinaryExpr $1 BopOr $3 }
|	x '^' x								{ BinaryExpr $1 BopBitXor $3 }
|	x '|' x								{ BinaryExpr $1 BopBitOr $3 }
|	x '&&' x							{ BinaryExpr $1 BopAnd $3 }
|	x '&' x								{ BinaryExpr $1 BopBitAnd $3 }
|	x '/' x								{ BinaryExpr $1 BopDiv $3 }
|	x '*' x								{ BinaryExpr $1 BopMul $3 }
|	x '%' x								{ BinaryExpr $1 BopMod $3 }
|	x '+' x								{ BinaryExpr $1 BopPlus $3 }
|	x '-' x								{ BinaryExpr $1 BopMinus $3 }
|	x '<' x								{ BinaryExpr $1 BopLt $3 }
|	x '<=' x							{ BinaryExpr $1 BopLe $3 }
|	x '<<' x							{ BinaryExpr $1 BopLsh $3 }
|	x '>' x								{ BinaryExpr $1 BopGt $3 }
|	x '>=' x							{ BinaryExpr $1 BopGe $3 }
|	x '>>' x							{ BinaryExpr $1 BopRsh $3 }
|	x '?' x ':' x							{ TernaryExpr $1 $3 $5 }
|	'(' x ')'							{ ParenExpr $2 }
|	'!' x								{ UnaryExpr UopNot $2 }
|	'~' x								{ UnaryExpr UopNeg $2 }
|	'-' x %prec NEG							{ UnaryExpr UopMinus $2 }
|	'&' x %prec ADDRESS						{ UnaryExpr UopAddress $2 }
|	'(' QualType ')' x %prec CAST					{ CastExpr $2 $4 }
|	sizeof '(' x ')'						{ SizeofExpr $3 }
|	sizeof '(' Type ')'						{ SizeofExpr $3 }

LiteralExpr :: { Node }
LiteralExpr
:	LIT_CHAR							{ LiteralExpr Char $1 }
|	LIT_INTEGER							{ LiteralExpr Int $1 }
|	LIT_FALSE							{ LiteralExpr Bool $1 }
|	LIT_TRUE							{ LiteralExpr Bool $1 }
|	LIT_STRING							{ LiteralExpr String $1 }
|	ID_CONST							{ LiteralExpr ConstId $1 }

Expr :: { Node }
Expr
:	LhsExpr								{ $1 }
|	ExprStmt							{ $1 }
|	LiteralExpr							{ $1 }
|	FunctionCall							{ $1 }
|	PureExpr(Expr)							{ $1 }

AssignExpr :: { Node }
AssignExpr
:	LhsExpr AssignOperator Expr					{ AssignExpr $1 $2 $3 }

AssignOperator :: { AssignOp }
AssignOperator
:	'='								{ AopEq      }
|	'*='								{ AopMul     }
|	'/='								{ AopDiv     }
|	'+='								{ AopPlus    }
|	'-='								{ AopMinus   }
|	'&='								{ AopBitAnd  }
|	'|='								{ AopBitOr   }
|	'^='								{ AopBitXor  }
|	'%='								{ AopMod     }
|	'<<='								{ AopLsh     }
|	'>>='								{ AopRsh     }

ExprStmt :: { Node }
ExprStmt
:	'++' Expr							{ UnaryExpr UopIncr $2 }
|	'--' Expr							{ UnaryExpr UopDecr $2 }

LhsExpr :: { Node }
LhsExpr
:	ID_VAR								{ VarExpr $1 }
|	'*' LhsExpr %prec DEREF						{ UnaryExpr UopDeref $2 }
|	LhsExpr '.' ID_VAR						{ MemberAccess $1 $3 }
|	LhsExpr '->' ID_VAR						{ PointerAccess $1 $3 }
|	LhsExpr '[' Expr ']'						{ ArrayAccess $1 $3 }

FunctionCall :: { Node }
FunctionCall
:	Expr ArgList							{ FunctionCall $1 $2 }

ArgList :: { [Node] }
ArgList
:	'(' ')'								{ [] }
|	'(' Args ')'							{ reverse $2 }

Args :: { [Node] }
Args
:	Arg								{ [$1] }
|	Args ',' Arg							{ $3 : $1 }

Arg :: { Node }
Arg
:	Expr								{ $1 }
|	Comment Expr							{ CommentExpr $1 $2 }

EnumDecl :: { Node }
EnumDecl
:	typedef enum ID_SUE_TYPE EnumeratorList ID_SUE_TYPE ';'		{ EnumDecl $3 $4 $5 }

EnumeratorList :: { [Node] }
EnumeratorList
:	'{' Enumerators '}'						{ $2 }

Enumerators :: { [Node] }
Enumerators
:	Enumerator							{ [$1] }
|	Enumerators Enumerator						{ $2 : $1 }

Enumerator :: { Node }
Enumerator
:	ID_CONST ','							{ Enumerator $1 Nothing }
|	ID_CONST '=' ConstExpr ','					{ Enumerator $1 (Just $3) }
|	Comment								{ $1 }

AggregateDecl :: { Node }
AggregateDecl
:	AggregateType ';'						{ $1 }
|	typedef AggregateType ID_SUE_TYPE ';'				{ Typedef $2 $3 }

AggregateType :: { Node }
AggregateType
:	struct ID_SUE_TYPE '{' MemberDecls '}'				{ Struct $2 $4 }
|	union ID_SUE_TYPE '{' MemberDecls '}'				{ Union $2 $4 }

MemberDecls :: { [Node] }
MemberDecls
:	MemberDecl							{ [$1] }
|	MemberDecls MemberDecl						{ $2 : $1 }

MemberDecl :: { Node }
MemberDecl
:	QualType DeclSpec(ConstExpr) ';'				{ MemberDecl $1 $2 Nothing }
|	QualType DeclSpec(ConstExpr) ':' LIT_INTEGER ';'		{ MemberDecl $1 $2 (Just $4) }
|	PreprocIfdef(MemberDecls)					{ $1 }
|	Comment								{ $1 }

TypedefDecl :: { Node }
TypedefDecl
:	typedef QualType ID_SUE_TYPE ';'				{ Typedef $2 $3 }
|	typedef FunctionPrototype(ID_FUNC_TYPE) ';'			{ TypedefFunction $2 }

QualType :: { Node }
QualType
:	Type								{ $1 }
|	const Type							{ TyConst $2 }

Type :: { Node }
Type
:	LeafType							{ $1 }
|	Type '*'							{ TyPointer $1 }
|	Type const							{ TyConst $1 }

LeafType :: { Node }
LeafType
:	struct ID_SUE_TYPE						{ TyStruct $2 }
|	void								{ TyStd $1 }
|	ID_FUNC_TYPE							{ TyFunc $1 }
|	ID_STD_TYPE							{ TyStd $1 }
|	ID_SUE_TYPE							{ TyUserDefined $1 }

FunctionDecl :: { Node }
FunctionDecl
:	FunctionDeclarator						{ $1 Global }
|	static FunctionDeclarator					{ $2 Static }

FunctionDeclarator :: { Scope -> Node }
FunctionDeclarator
:	FunctionPrototype(ID_VAR) ';'					{ \s -> FunctionDecl s $1 }
|	FunctionPrototype(ID_VAR) CompoundStmt				{ \s -> FunctionDefn s $1 $2 }

FunctionPrototype(id)
:	QualType id FunctionParamList					{ FunctionPrototype $1 $2 $3 }

FunctionParamList :: { [Node] }
FunctionParamList
:	'(' void ')'							{ [] }
|	'(' FunctionParams ')'						{ reverse $2 }
|	'(' FunctionParams ',' '...' ')'				{ reverse $ Ellipsis : $2 }

FunctionParams :: { [Node] }
FunctionParams
:	FunctionParam							{ [$1] }
|	FunctionParams ',' FunctionParam				{ $3 : $1 }

FunctionParam :: { Node }
FunctionParam
:	QualType DeclSpec(ConstExpr)					{ FunctionParam $1 $2 }

ConstDecl :: { Node }
ConstDecl
:	extern const LeafType ID_VAR ';'				{ ConstDecl $3 $4 }
|	const LeafType ID_VAR '=' InitialiserExpr ';'			{ ConstDefn Global $2 $3 $5 }
|	static const LeafType ID_VAR '=' InitialiserExpr ';'		{ ConstDefn Static $3 $4 $6 }

{
parseError :: Lexeme -> Alex a
parseError = fail . show

lexwrap :: (Lexeme -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)
}
