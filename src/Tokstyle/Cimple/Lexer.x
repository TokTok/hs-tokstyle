{
module Tokstyle.Cimple.Lexer
    ( Alex
    , Lexeme (..)
    , LexemeClass (..)
    , alexScanTokens
    , alexMonadScan
    , runAlex
    ) where
}

%wrapper "monad"

tokens :-

-- SPDX comments.
<0>		"/* SPDX-License-Identifier: "		{ mkL SpdxLicense `andBegin` spdxValueSC }

<spdxSC>	" * Copyright © "			{ mkL SpdxCopyright `andBegin` spdxValueSC }
<spdxSC>	" */"\n					{ start 0 }

<spdxValueSC>	[0-9]{4}("-"[0-9]{4})?			;
<spdxValueSC>	[^\n]					;
<spdxValueSC>	\n					{ mkL PpNewline `andBegin` spdxSC }

-- Ignore attributes.
<0,ppSC>	"GNU_PRINTF("[^\)]+")"			;
<0,ppSC>	"VLA"					{ mkL KwVla }

-- Winapi functions.
<0,ppSC>	"WSAAddressToString"			{ mkL IdVar }
<0,ppSC>	"LocalFree"				{ mkL IdVar }
<0,ppSC>	"FormatMessageA"			{ mkL IdVar }
<0,ppSC>	"WSAGetLastError"			{ mkL IdVar }
<0,ppSC>	"WSAStringToAddress"			{ mkL IdVar }
<0,ppSC>	"WSAStartup"				{ mkL IdVar }
<0,ppSC>	"GetAdaptersInfo"			{ mkL IdVar }
<0,ppSC>	"WSACleanup"				{ mkL IdVar }
<0,ppSC>	"GetSystemTimeAsFileTime"		{ mkL IdVar }
<0,ppSC>	"GetTickCount"				{ mkL IdVar }

-- Winapi struct members.
<0,ppSC>	"GatewayList"				{ mkL IdVar }
<0,ppSC>	"Next"					{ mkL IdVar }
<0,ppSC>	"IpAddress"				{ mkL IdVar }
<0,ppSC>	"IpAddressList"				{ mkL IdVar }
<0,ppSC>	"IpMask"				{ mkL IdVar }
<0,ppSC>	"String"				{ mkL IdVar }

-- Windows typedefs.
<0,ppSC>	"DWORD"					{ mkL IdStdType }
<0,ppSC>	"FILETIME"				{ mkL IdStdType }
<0,ppSC>	"INT"					{ mkL IdStdType }
<0,ppSC>	"LPSOCKADDR"				{ mkL IdStdType }
<0,ppSC>	"IP_ADAPTER_INFO"			{ mkL IdStdType }
<0,ppSC>	"LPTSTR"				{ mkL IdStdType }
<0,ppSC>	"u_long"				{ mkL IdStdType }
<0,ppSC>	"WSADATA"				{ mkL IdStdType }

-- System struct types.
<0,ppSC>	"addrinfo"				{ mkL IdSueType }
<0,ppSC>	"ifconf"				{ mkL IdSueType }
<0,ppSC>	"ifreq"					{ mkL IdSueType }
<0,ppSC>	"epoll_event"				{ mkL IdSueType }
<0,ppSC>	"in_addr"				{ mkL IdSueType }
<0,ppSC>	"in6_addr"				{ mkL IdSueType }
<0,ppSC>	"ipv6_mreq"				{ mkL IdSueType }
<0,ppSC>	"sockaddr"				{ mkL IdSueType }
<0,ppSC>	"sockaddr_in"				{ mkL IdSueType }
<0,ppSC>	"sockaddr_in6"				{ mkL IdSueType }
<0,ppSC>	"sockaddr_storage"			{ mkL IdSueType }
<0,ppSC>	"timespec"				{ mkL IdSueType }
<0,ppSC>	"timeval"				{ mkL IdSueType }

-- Sodium constants.
<0,ppSC>	"crypto_auth_"[A-Z][A-Z0-9_]*		{ mkL IdConst }
<0,ppSC>	"crypto_box_"[A-Z][A-Z0-9_]*		{ mkL IdConst }
<0,ppSC>	"crypto_hash_sha256_"[A-Z][A-Z0-9_]*	{ mkL IdConst }
<0,ppSC>	"crypto_hash_sha512_"[A-Z][A-Z0-9_]*	{ mkL IdConst }
<0,ppSC>	"crypto_sign_"[A-Z][A-Z0-9_]*		{ mkL IdConst }
<0,ppSC>	"MAX"					{ mkL IdConst }
<0,ppSC>	"MIN"					{ mkL IdConst }

-- Standard C (ish).
<ppSC>		defined					{ mkL PpDefined }
<ppSC>		\"[^\"]*\"				{ mkL LitString }
<ppSC>		\n					{ mkL PpNewline `andBegin` 0 }
<ppSC>		\\\n					;
<ppSC>		$white					;

<commentSC>	"*/"					{ start 0 }
<commentSC>	[.\n]					;

<dstringSC>	\"					{ start 0 }
<dstringSC>	\\\\|\\[^"]|[^\\]|\n			;

<sstringSC>	'					{ start 0 }
<sstringSC>	\\\\|\\'|[^\\]|\n			;

<ignoreSC>	"//!TOKSTYLE+"				{ start 0 }
<ignoreSC>	[.\n]					;

<0,ppSC>	"//"\n					;
<0,ppSC>	"// ".*					;
<0>		$white+					;
<0>		"//!TOKSTYLE-"				{ start ignoreSC }
<0>		"/*"					{ start commentSC }
<0>		\"					{ mkL LitString `andBegin` dstringSC }
<0>		'					{ mkL LitChar `andBegin` sstringSC }
<0>		"<"[a-z0-9\.\/_]+">"			{ mkL LitSysInclude }
<0>		"#if"					{ mkL PpIf `andBegin` ppSC }
<0>		"#ifdef"				{ mkL PpIfdef }
<0>		"#ifndef"				{ mkL PpIfndef }
<0>		"#elif"					{ mkL PpElif `andBegin` ppSC }
<0>		"#else"					{ mkL PpElse }
<0>		"#endif"				{ mkL PpEndif }
<0>		"#define"				{ mkL PpDefine `andBegin` ppSC }
<0>		"#undef"				{ mkL PpUndef }
<0>		"#include"				{ mkL PpInclude }
<0>		"#error"				{ mkL PpError }
<0,ppSC>	"break"					{ mkL KwBreak }
<0,ppSC>	"case"					{ mkL KwCase }
<0,ppSC>	"const"					{ mkL KwConst }
<0,ppSC>	"continue"				{ mkL KwContinue }
<0,ppSC>	"default"				{ mkL KwDefault }
<0,ppSC>	"do"					{ mkL KwDo }
<0,ppSC>	"else"					{ mkL KwElse }
<0,ppSC>	"enum"					{ mkL KwEnum }
<0,ppSC>	"extern"				{ mkL KwExtern }
<0,ppSC>	"for"					{ mkL KwFor }
<0,ppSC>	"goto"					{ mkL KwGoto }
<0,ppSC>	"if"					{ mkL KwIf }
<0,ppSC>	"return"				{ mkL KwReturn }
<0,ppSC>	"sizeof"				{ mkL KwSizeof }
<0,ppSC>	"static"				{ mkL KwStatic }
<0,ppSC>	"struct"				{ mkL KwStruct }
<0,ppSC>	"switch"				{ mkL KwSwitch }
<0,ppSC>	"typedef"				{ mkL KwTypedef }
<0,ppSC>	"union"					{ mkL KwUnion }
<0,ppSC>	"void"					{ mkL KwVoid }
<0,ppSC>	"while"					{ mkL KwWhile }
<0,ppSC>	"bool"					{ mkL IdStdType }
<0,ppSC>	"char"					{ mkL IdStdType }
<0,ppSC>	"double"				{ mkL IdStdType }
<0,ppSC>	"float"					{ mkL IdStdType }
<0,ppSC>	"int"					{ mkL IdStdType }
<0,ppSC>	"long int"				{ mkL IdStdType }
<0,ppSC>	"long signed int"			{ mkL IdStdType }
<0,ppSC>	"long"					{ mkL IdStdType }
<0,ppSC>	"signed int"				{ mkL IdStdType }
<0,ppSC>	"unsigned int"				{ mkL IdStdType }
<0,ppSC>	"unsigned long"				{ mkL IdStdType }
<0,ppSC>	"unsigned"				{ mkL IdStdType }
<0,ppSC>	"va_list"				{ mkL IdStdType }
<0,ppSC>	"__func__"				{ mkL IdVar }
<0,ppSC>	"__"[a-zA-Z]+"__"?			{ mkL IdConst }
<0,ppSC>	"true"					{ mkL LitInteger }
<0,ppSC>	"false"					{ mkL LitInteger }
<0,ppSC>	[A-Z][A-Z0-9_]{1,2}			{ mkL IdSueType }
<0,ppSC>	_*[A-Z][A-Z0-9_]*			{ mkL IdConst }
<0,ppSC>	[A-Z][A-Za-z0-9_]*[a-z][A-Za-z0-9_]*	{ mkL IdSueType }
<0,ppSC>	[a-z][a-z0-9_]*_t			{ mkL IdStdType }
<0,ppSC>	[a-z][a-z0-9_]*_cb			{ mkL IdFuncType }
<0,ppSC>	[a-z][A-Za-z0-9_]*			{ mkL IdVar }
<0,ppSC>	[0-9]+[LU]*				{ mkL LitInteger }
<0,ppSC>	[0-9]+"."[0-9]+f?			{ mkL LitInteger }
<0,ppSC>	0x[0-9a-fA-F]+[LU]*			{ mkL LitInteger }
<0,ppSC>	"="					{ mkL PctEq }
<0,ppSC>	"=="					{ mkL PctEqEq }
<0,ppSC>	"&"					{ mkL PctAmpersand }
<0,ppSC>	"&&"					{ mkL PctAmpersandAmpersand }
<0,ppSC>	"&="					{ mkL PctAmpersandEq }
<0,ppSC>	"->"					{ mkL PctArrow }
<0,ppSC>	","					{ mkL PctComma }
<0,ppSC>	"+"					{ mkL PctPlus }
<0,ppSC>	"++"					{ mkL PctPlusPlus }
<0,ppSC>	"+="					{ mkL PctPlusEq }
<0,ppSC>	"-"					{ mkL PctMinus }
<0,ppSC>	"--"					{ mkL PctMinusMinus }
<0,ppSC>	"-="					{ mkL PctMinusEq }
<0,ppSC>	"~"					{ mkL PctTilde }
<0,ppSC>	"/"					{ mkL PctSlash }
<0,ppSC>	"/="					{ mkL PctSlashEq }
<0,ppSC>	"."					{ mkL PctPeriod }
<0,ppSC>	"..."					{ mkL PctEllipsis }
<0,ppSC>	"%"					{ mkL PctPercent }
<0,ppSC>	"%="					{ mkL PctPercentEq }
<0,ppSC>	";"					{ mkL PctSemicolon }
<0,ppSC>	":"					{ mkL PctColon }
<0,ppSC>	"<"					{ mkL PctLess }
<0,ppSC>	"<<"					{ mkL PctLessLess }
<0,ppSC>	"<<="					{ mkL PctLessLessEq }
<0,ppSC>	"<="					{ mkL PctLessEq }
<0,ppSC>	">"					{ mkL PctGreater }
<0,ppSC>	">>"					{ mkL PctGreaterGreater }
<0,ppSC>	">>="					{ mkL PctGreaterGreaterEq }
<0,ppSC>	">="					{ mkL PctGreaterEq }
<0,ppSC>	"|"					{ mkL PctPipe }
<0,ppSC>	"||"					{ mkL PctPipePipe }
<0,ppSC>	"|="					{ mkL PctPipeEq }
<0,ppSC>	"["					{ mkL PctLBrack }
<0,ppSC>	"]"					{ mkL PctRBrack }
<0,ppSC>	"{"					{ mkL PctLBrace }
<0,ppSC>	"}"					{ mkL PctRBrace }
<0,ppSC>	"("					{ mkL PctLParen }
<0,ppSC>	")"					{ mkL PctRParen }
<0,ppSC>	"?"					{ mkL PctQMark }
<0,ppSC>	"!"					{ mkL PctEMark }
<0,ppSC>	"!="					{ mkL PctEMarkEq }
<0,ppSC>	"*"					{ mkL PctAsterisk }
<0,ppSC>	"*="					{ mkL PctAsteriskEq }
<0,ppSC>	"^"					{ mkL PctCaret }
<0,ppSC>	"^="					{ mkL PctCaretEq }

<0,ppSC,spdxSC>	.					{ mkL Error }

{
data LexemeClass
    = Comment
    | IdConst
    | IdFuncType
    | IdStdType
    | IdSueType
    | IdVar
    | KwBreak
    | KwCase
    | KwConst
    | KwContinue
    | KwDefault
    | KwDo
    | KwFor
    | KwGoto
    | KwIf
    | KwElse
    | KwEnum
    | KwExtern
    | KwReturn
    | KwSizeof
    | KwStatic
    | KwStruct
    | KwSwitch
    | KwTypedef
    | KwUnion
    | KwVla
    | KwVoid
    | KwWhile
    | LitChar
    | LitInteger
    | LitString
    | LitSysInclude
    | PctAmpersand
    | PctAmpersandAmpersand
    | PctAmpersandEq
    | PctArrow
    | PctAsterisk
    | PctAsteriskEq
    | PctCaret
    | PctCaretEq
    | PctColon
    | PctComma
    | PctEllipsis
    | PctEMark
    | PctEMarkEq
    | PctEq
    | PctEqEq
    | PctGreater
    | PctGreaterEq
    | PctGreaterGreater
    | PctGreaterGreaterEq
    | PctLBrace
    | PctLBrack
    | PctLess
    | PctLessEq
    | PctLessLess
    | PctLessLessEq
    | PctLParen
    | PctMinus
    | PctMinusEq
    | PctMinusMinus
    | PctPeriod
    | PctPercent
    | PctPercentEq
    | PctPipe
    | PctPipeEq
    | PctPipePipe
    | PctPlus
    | PctPlusEq
    | PctPlusPlus
    | PctQMark
    | PctRBrace
    | PctRBrack
    | PctRParen
    | PctSemicolon
    | PctSlash
    | PctSlashEq
    | PctTilde
    | PpDefine
    | PpDefined
    | PpElif
    | PpElse
    | PpEndif
    | PpError
    | PpIf
    | PpIfdef
    | PpIfndef
    | PpInclude
    | PpNewline
    | PpUndef
    | SpdxCopyright
    | SpdxLicense

    | Error
    | Eof
    deriving (Show, Eq)

data Lexeme = L AlexPosn LexemeClass String
    deriving (Show)

mkL :: LexemeClass -> AlexInput -> Int -> Alex Lexeme
mkL c (p, _, _, str) len = return (L p c (take len str))

start :: Int -> AlexInput -> Int -> Alex Lexeme
start code _ _ = do
    alexSetStartCode code
    alexMonadScan

alexEOF :: Alex Lexeme
alexEOF = return (L (AlexPn 0 0 0) Eof "")

alexScanTokens :: String -> Either String [Lexeme]
alexScanTokens str =
    runAlex str $ loop []
  where
    loop toks = do
        tok@(L _ cl _) <- alexMonadScan
        if cl == Eof
            then return $ reverse toks
            else loop $! (tok:toks)

}
