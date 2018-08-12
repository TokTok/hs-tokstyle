{
module Tokstyle.Cimple.Parser where

import           Tokstyle.Cimple.Lexer (Alex, Lexeme (..), LexemeClass (..),
                                        alexMonadScan)
}

-- Conflict between (static) FunctionDecl and (static) ConstDecl.
%expect 2

%name parseCimple
%error {parseError}
%lexer {lexwrap} {L undefined Eof ""}
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

TranslationUnit :: { [()] }
TranslationUnit
:	ToplevelDecls							{ $1 }

ToplevelDecls :: { [()] }
ToplevelDecls
:	ToplevelDecl							{ [$1] }
|	ToplevelDecls ToplevelDecl					{ $2 : $1 }

ToplevelDecl :: { () }
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

PreprocIfdef(decls)
:	'#ifdef' ID_CONST decls PreprocElse(decls) '#endif'		{ () }
|	'#ifndef' ID_CONST decls PreprocElse(decls) '#endif'		{ () }

PreprocIf(decls)
:	'#if' ConstExpr '\n' decls PreprocElse(decls) '#endif'		{ () }

PreprocElse(decls)
:									{ () }
|	'#else' decls							{ () }
|	'#elif' ConstExpr '\n' decls PreprocElse(decls)			{ () }

PreprocError :: { () }
PreprocError
:	'#error' LIT_STRING						{ () }

PreprocInclude :: { () }
PreprocInclude
:	'#include' LIT_STRING						{ () }
|	'#include' LIT_SYS_INCLUDE					{ () }

PreprocDefine :: { () }
PreprocDefine
:	'#define' ID_CONST ConstExpr '\n'				{ () }
|	'#define' ID_CONST '\n'						{ () }
|	'#define' ID_CONST MacroParamList MacroBody '\n'		{ () }

PreprocUndef :: { () }
PreprocUndef
:	'#undef' ID_CONST						{ () }

ConstExpr :: { () }
ConstExpr
:	LiteralExpr							{ () }
|	'defined' '(' ID_CONST ')'					{ () }
|	PureExpr(ConstExpr)						{ () }

MacroParamList :: { () }
MacroParamList
:	'(' ')'								{ () }
|	'(' MacroParams ')'						{ () }
|	'(' MacroParams ',' '...' ')'					{ () }

MacroParams :: { [()] }
MacroParams
:	MacroParam							{ [$1] }
|	MacroParams ',' MacroParam					{ $3 : $1 }

MacroParam :: { () }
MacroParam
:	ID_VAR								{ () }

MacroBody :: { () }
MacroBody
:	do CompoundStmt while '(' LIT_INTEGER ')'			{ () }
|	FunctionCall							{ () }

ExternC :: { () }
ExternC
:	'#ifdef' ID_CONST
	extern LIT_STRING '{'
	'#endif'
	ToplevelDecls
	'#ifdef' ID_CONST
	'}'
	'#endif'							{ () }

Stmts :: { [()] }
Stmts
:	Stmt								{ [$1] }
|	Stmts Stmt							{ $2 : $1 }

Stmt :: { () }
Stmt
:	PreprocIfdef(Stmts)						{ () }
|	PreprocIf(Stmts)						{ () }
|	PreprocDefine Stmts PreprocUndef				{ () }
|	LabelStmt							{ () }
|	DeclStmt							{ () }
|	CompoundStmt							{ () }
|	IfStmt								{ () }
|	ForStmt								{ () }
|	WhileStmt							{ () }
|	DoWhileStmt							{ () }
|	AssignExpr ';'							{ () }
|	ExprStmt ';'							{ () }
|	FunctionCall ';'						{ () }
|	break ';'							{ () }
|	goto ID_CONST ';'						{ () }
|	continue ';'							{ () }
|	return ';'							{ () }
|	return Expr ';'							{ () }
|	switch '(' Expr ')' CompoundStmt				{ () }

IfStmt :: { () }
IfStmt
:	if '(' Expr ')' CompoundStmt					{ () }
|	if '(' Expr ')' CompoundStmt else IfStmt			{ () }
|	if '(' Expr ')' CompoundStmt else CompoundStmt			{ () }

ForStmt :: { () }
ForStmt
:	for '(' ForInit Opt(Expr) ';' Opt(ForNext) ')' CompoundStmt	{ () }

ForInit :: { () }
ForInit
:	';'								{ () }
|	AssignExpr ';'							{ () }
|	SingleVarDecl							{ () }

ForNext :: { () }
ForNext
:	ExprStmt							{ () }
|	AssignExpr							{ () }

Opt(x)
:									{ () }
|	x								{ () }

WhileStmt :: { () }
WhileStmt
:	while '(' Expr ')' CompoundStmt					{ () }

DoWhileStmt :: { () }
DoWhileStmt
:	do CompoundStmt while '(' Expr ')' ';'				{ () }

LabelStmt :: { () }
LabelStmt
:	case Expr ':' Stmt						{ () }
|	default ':' Stmt						{ () }
|	ID_CONST ':' Stmt						{ () }

DeclStmt :: { () }
DeclStmt
:	VarDecl								{ () }
|	VLA '(' Type ',' ID_VAR ',' Expr ')' ';'			{ () }

SingleVarDecl :: { () }
SingleVarDecl
:	QualType Declarator ';'						{ () }

VarDecl :: { () }
VarDecl
:	QualType Declarators ';'					{ () }

Declarators :: { [()] }
Declarators
:	Declarator							{ [$1] }
|	Declarators ',' Declarator					{ $3 : $1 }

Declarator :: { () }
Declarator
:	DeclSpec(Expr) '=' InitialiserExpr				{ () }
|	DeclSpec(Expr)							{ () }

InitialiserExpr :: { () }
InitialiserExpr
:	InitialiserList							{ () }
|	Expr								{ () }

DeclSpec(expr)
:	ID_VAR								{ () }
|	DeclSpec(expr) '[' ']'						{ () }
|	DeclSpec(expr) '[' expr ']'					{ () }

InitialiserList :: { () }
InitialiserList
:	'{' Initialisers '}'						{ () }
|	'{' Initialisers ',' '}'					{ () }

Initialisers :: { [()] }
Initialisers
:	Initialiser							{ [$1] }
|	Initialisers ',' Initialiser					{ $3 : $1 }

Initialiser :: { () }
Initialiser
:	Expr								{ () }
|	InitialiserList							{ () }

CompoundStmt :: { () }
CompoundStmt
:	'{' Stmts '}'							{ () }

PureExpr(x)
:	x '!=' x							{ () }
|	x '==' x							{ () }
|	x '||' x							{ () }
|	x '^' x								{ () }
|	x '|' x								{ () }
|	x '&&' x							{ () }
|	x '&' x								{ () }
|	x '/' x								{ () }
|	x '*' x								{ () }
|	x '%' x								{ () }
|	x '+' x								{ () }
|	x '-' x								{ () }
|	x '<' x								{ () }
|	x '<=' x							{ () }
|	x '<<' x							{ () }
|	x '>' x								{ () }
|	x '>=' x							{ () }
|	x '>>' x							{ () }
|	x '?' x ':' x							{ () }
|	'(' x ')'							{ () }
|	'!' x								{ () }
|	'~' x								{ () }
|	'-' x %prec NEG							{ () }
|	'&' x %prec ADDRESS						{ () }
|	'(' QualType ')' x %prec CAST					{ () }
|	sizeof '(' x ')'						{ () }
|	sizeof '(' Type ')'						{ () }

LiteralExpr :: { () }
LiteralExpr
:	LIT_CHAR							{ () }
|	LIT_INTEGER							{ () }
|	LIT_STRING							{ () }
|	ID_CONST							{ () }

Expr :: { () }
Expr
:	LhsExpr								{ () }
|	ExprStmt							{ () }
|	LiteralExpr							{ () }
|	FunctionCall							{ () }
|	PureExpr(Expr)							{ () }

AssignExpr :: { () }
AssignExpr
:	LhsExpr AssignOperator Expr					{ () }

AssignOperator :: { () }
AssignOperator
:	'='								{ () }
|	'*='								{ () }
|	'/='								{ () }
|	'+='								{ () }
|	'-='								{ () }
|	'&='								{ () }
|	'|='								{ () }
|	'^='								{ () }
|	'%='								{ () }
|	'<<='								{ () }
|	'>>='								{ () }

ExprStmt :: { () }
ExprStmt
:	'++' Expr							{ () }
|	'--' Expr							{ () }

LhsExpr :: { () }
LhsExpr
:	ID_VAR								{ () }
|	'*' LhsExpr %prec DEREF						{ () }
-- TODO(iphydf): We don't want this, it's most likely a bug:
|	'*' '(' QualType ')' LhsExpr %prec DEREF			{ () }
|	LhsExpr '.' ID_VAR						{ () }
|	LhsExpr '->' ID_VAR						{ () }
|	LhsExpr '[' Expr ']'						{ () }

FunctionCall :: { () }
FunctionCall
:	Expr ArgList							{ () }

ArgList :: { () }
ArgList
:	'(' ')'								{ () }
|	'(' Args ')'							{ () }

Args :: { [()] }
Args
:	Expr								{ [$1] }
|	Args ',' Expr							{ $3 : $1 }

EnumDecl :: { () }
EnumDecl
:	typedef enum ID_SUE_TYPE EnumeratorList ID_SUE_TYPE ';'		{ () }

EnumeratorList :: { () }
EnumeratorList
:	'{' Enumerators '}'						{ () }
|	'{' Enumerators ',' '}'						{ () }

Enumerators :: { [()] }
Enumerators
:	Enumerator							{ [$1] }
|	Enumerators ',' Enumerator					{ $3 : $1 }

Enumerator :: { () }
Enumerator
:	ID_CONST							{ () }
|	ID_CONST '=' ConstExpr						{ () }

AggregateDecl :: { () }
AggregateDecl
:	AggregateType ';'						{ () }
|	typedef AggregateType ID_SUE_TYPE ';'				{ () }

AggregateType :: { () }
AggregateType
:	struct ID_SUE_TYPE '{' MemberDecls '}'				{ () }
|	union ID_SUE_TYPE '{' MemberDecls '}'				{ () }

MemberDecls :: { [()] }
MemberDecls
:	MemberDecl							{ [$1] }
|	MemberDecls MemberDecl						{ $2 : $1 }

MemberDecl :: { () }
MemberDecl
:	QualType DeclSpec(ConstExpr) ';'				{ () }
|	QualType DeclSpec(ConstExpr) ':' LIT_INTEGER ';'		{ () }
|	PreprocIfdef(MemberDecls)					{ () }

TypedefDecl :: { () }
TypedefDecl
:	typedef QualType ID_SUE_TYPE ';'				{ () }
|	typedef FunctionPrototype(ID_FUNC_TYPE) ';'			{ () }

QualType :: { () }
QualType
:	Type								{ () }
|	const Type							{ () }

Type :: { () }
Type
:	LeafType							{ () }
|	Type '*'							{ () }
|	Type const							{ () }

LeafType :: { () }
LeafType
:	struct ID_SUE_TYPE						{ () }
|	void								{ () }
|	ID_FUNC_TYPE							{ () }
|	ID_STD_TYPE							{ () }
|	ID_SUE_TYPE							{ () }

FunctionDecl :: { () }
FunctionDecl
:	FunctionDeclarator						{ () }
|	static FunctionDeclarator					{ () }

FunctionDeclarator
:	QualType ID_VAR ';'						{ () }
|	FunctionPrototype(ID_VAR) FunctionBody				{ () }

FunctionPrototype(id)
:	QualType id FunctionParamList					{ () }

FunctionBody :: { () }
FunctionBody
:	';'								{ () }
|	CompoundStmt							{ () }

FunctionParamList :: { () }
FunctionParamList
:	'(' void ')'							{ () }
|	'(' FunctionParams ')'						{ () }
|	'(' FunctionParams ',' '...' ')'				{ () }

FunctionParams :: { [()] }
FunctionParams
:	FunctionParam							{ [$1] }
|	FunctionParams ',' FunctionParam				{ $3 : $1 }

FunctionParam :: { () }
FunctionParam
:	QualType DeclSpec(ConstExpr)					{ () }

ConstDecl :: { () }
ConstDecl
:	extern const LeafType ID_VAR ';'				{ () }
|	const LeafType ID_VAR '=' InitialiserExpr ';'			{ () }
|	static const LeafType ID_VAR '=' InitialiserExpr ';'		{ () }

{
parseError :: Lexeme -> Alex a
parseError = fail . show

lexwrap :: (Lexeme -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)
}
