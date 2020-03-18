{
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
module Tokstyle.Cimple.Lexer
    ( Alex
    , AlexPosn (..)
    , alexScanTokens
    , alexMonadScan
    , Lexeme (..)
    , lexemeClass
    , lexemePosn
    , lexemeText
    , lexemeLine
    , mkL
    , runAlex
    ) where

import           Tokstyle.Cimple.Tokens (LexemeClass (..))
}

%wrapper "monad"

tokens :-

-- Ignore attributes.
<0>		"GNU_PRINTF("[^\)]+")"			;
<0>		"VLA"					{ mkL KwVla }

-- Winapi functions.
<0>		"WSAAddressToString"			{ mkL IdVar }
<0>		"LocalFree"				{ mkL IdVar }
<0>		"FormatMessageA"			{ mkL IdVar }
<0>		"WSAGetLastError"			{ mkL IdVar }
<0>		"WSAStringToAddress"			{ mkL IdVar }
<0>		"WSAStartup"				{ mkL IdVar }
<0>		"GetAdaptersInfo"			{ mkL IdVar }
<0>		"WSACleanup"				{ mkL IdVar }
<0>		"GetSystemTimeAsFileTime"		{ mkL IdVar }
<0>		"GetTickCount"				{ mkL IdVar }

-- Winapi struct members.
<0>		"GatewayList"				{ mkL IdVar }
<0>		"Next"					{ mkL IdVar }
<0>		"IpAddress"				{ mkL IdVar }
<0>		"IpAddressList"				{ mkL IdVar }
<0>		"IpMask"				{ mkL IdVar }
<0>		"String"				{ mkL IdVar }

-- Windows typedefs.
<0>		"DWORD"					{ mkL IdStdType }
<0>		"FILETIME"				{ mkL IdStdType }
<0>		"INT"					{ mkL IdStdType }
<0>		"LPSOCKADDR"				{ mkL IdStdType }
<0>		"IP_ADAPTER_INFO"			{ mkL IdStdType }
<0>		"LPTSTR"				{ mkL IdStdType }
<0>		"u_long"				{ mkL IdStdType }
<0>		"WSADATA"				{ mkL IdStdType }

-- System struct types.
<0>		"addrinfo"				{ mkL IdSueType }
<0>		"ifconf"				{ mkL IdSueType }
<0>		"ifreq"					{ mkL IdSueType }
<0>		"epoll_event"				{ mkL IdSueType }
<0>		"in_addr"				{ mkL IdSueType }
<0>		"in6_addr"				{ mkL IdSueType }
<0>		"ipv6_mreq"				{ mkL IdSueType }
<0>		"sockaddr"				{ mkL IdSueType }
<0>		"sockaddr_in"				{ mkL IdSueType }
<0>		"sockaddr_in6"				{ mkL IdSueType }
<0>		"sockaddr_storage"			{ mkL IdSueType }
<0>		"timespec"				{ mkL IdSueType }
<0>		"timeval"				{ mkL IdSueType }

-- Sodium constants.
<0,ppSC>	"crypto_auth_"[A-Z][A-Z0-9_]*		{ mkL IdConst }
<0,ppSC>	"crypto_box_"[A-Z][A-Z0-9_]*		{ mkL IdConst }
<0,ppSC>	"crypto_hash_sha256_"[A-Z][A-Z0-9_]*	{ mkL IdConst }
<0,ppSC>	"crypto_hash_sha512_"[A-Z][A-Z0-9_]*	{ mkL IdConst }
<0,ppSC>	"crypto_sign_"[A-Z][A-Z0-9_]*		{ mkL IdConst }
<0>		"MAX"					{ mkL IdConst }
<0>		"MIN"					{ mkL IdConst }

-- Standard C (ish).
<ppSC>		defined					{ mkL PpDefined }
<ppSC>		\"[^\"]*\"				{ mkL LitString }
<ppSC>		\n					{ mkL PpNewline `andBegin` 0 }
<ppSC>		\\\n					;
<ppSC>		$white					;

<ignoreSC>	"//!TOKSTYLE+"				{ start 0 }
<ignoreSC>	[.\n]					;

<0,ppSC>	"//"\n					;
<0,ppSC>	"// ".*					;
<0>		$white+					;
<0>		"//!TOKSTYLE-"				{ start ignoreSC }
<0>		"/*""*"?				{ mkL CmtStart `andBegin` cmtSC }
<0>		"/""*"+\n" *"\n" * :: ".+\n" *"\n" ""*"+"/"	{ mkL CmtBlock }
<0,cmtSC>	\"(\\.|[^\"])*\"			{ mkL LitString }
<0>		'(\\|[^'])*'				{ mkL LitChar }
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
<0,ppSC>	"false"					{ mkL LitFalse }
<0,ppSC>	"true"					{ mkL LitTrue }
<0,ppSC>	"__func__"				{ mkL IdVar }
<0,ppSC>	"__"[a-zA-Z]+"__"?			{ mkL IdConst }
<0,ppSC>	[A-Z][A-Z0-9_]{1,2}			{ mkL IdSueType }
<0,ppSC>	_*[A-Z][A-Z0-9_]*			{ mkL IdConst }
<0,ppSC>	[A-Z][A-Za-z0-9_]*[a-z][A-Za-z0-9_]*	{ mkL IdSueType }
<0,ppSC>	[a-z][a-z0-9_]*_t			{ mkL IdStdType }
<0,ppSC>	[a-z][a-z0-9_]*_cb			{ mkL IdFuncType }
<0,ppSC>	[a-z][A-Za-z0-9_]*			{ mkL IdVar }
<0,ppSC,cmtSC>	[0-9]+[LU]*				{ mkL LitInteger }
<0,ppSC>	[0-9]+"."[0-9]+f?			{ mkL LitInteger }
<0,ppSC>	0x[0-9a-fA-F]+[LU]*			{ mkL LitInteger }
<0,ppSC,cmtSC>	"="					{ mkL PctEq }
<0,ppSC>	"=="					{ mkL PctEqEq }
<0,ppSC>	"&"					{ mkL PctAmpersand }
<0,ppSC>	"&&"					{ mkL PctAmpersandAmpersand }
<0,ppSC>	"&="					{ mkL PctAmpersandEq }
<0,ppSC>	"->"					{ mkL PctArrow }
<0,ppSC,cmtSC>	","					{ mkL PctComma }
<0,ppSC,cmtSC>	"+"					{ mkL PctPlus }
<0,ppSC>	"++"					{ mkL PctPlusPlus }
<0,ppSC>	"+="					{ mkL PctPlusEq }
<0,ppSC,cmtSC>	"-"					{ mkL PctMinus }
<0,ppSC>	"--"					{ mkL PctMinusMinus }
<0,ppSC>	"-="					{ mkL PctMinusEq }
<0,ppSC>	"~"					{ mkL PctTilde }
<0,ppSC,cmtSC>	"/"					{ mkL PctSlash }
<0,ppSC>	"/="					{ mkL PctSlashEq }
<0,ppSC,cmtSC>	"."					{ mkL PctPeriod }
<0,ppSC>	"..."					{ mkL PctEllipsis }
<0,ppSC>	"%"					{ mkL PctPercent }
<0,ppSC>	"%="					{ mkL PctPercentEq }
<0,ppSC,cmtSC>	";"					{ mkL PctSemicolon }
<0,ppSC,cmtSC>	":"					{ mkL PctColon }
<0,ppSC,cmtSC>	"<"					{ mkL PctLess }
<0,ppSC>	"<<"					{ mkL PctLessLess }
<0,ppSC>	"<<="					{ mkL PctLessLessEq }
<0,ppSC>	"<="					{ mkL PctLessEq }
<0,ppSC,cmtSC>	">"					{ mkL PctGreater }
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
<0,ppSC,cmtSC>	"("					{ mkL PctLParen }
<0,ppSC,cmtSC>	")"					{ mkL PctRParen }
<0,ppSC,cmtSC>	"?"					{ mkL PctQMark }
<0,ppSC,cmtSC>	"!"					{ mkL PctEMark }
<0,ppSC>	"!="					{ mkL PctEMarkEq }
<0,ppSC>	"*"					{ mkL PctAsterisk }
<0,ppSC>	"*="					{ mkL PctAsteriskEq }
<0,ppSC>	"^"					{ mkL PctCaret }
<0,ppSC>	"^="					{ mkL PctCaretEq }

-- Comments.
<cmtSC>		"Copyright ©"				{ mkL CmtSpdxCopyright }
<cmtSC>		"SPDX-License-Identifier:"		{ mkL CmtSpdxLicense }
<cmtSC>		"GPL-3.0-or-later"			{ mkL CmtWord }
<cmtSC>		"TODO("[^\)]+"):"			{ mkL CmtWord }
<cmtSC>		[@\\][a-z]+				{ mkL CmtWord }
<cmtSC>		"*"[A-Za-z][A-Za-z0-9_']*"*"		{ mkL CmtWord }
<cmtSC>		[A-Za-z][A-Za-z0-9_']*			{ mkL CmtWord }
<cmtSC>		"#"[0-9]+				{ mkL CmtWord }
<cmtSC>		"http://"[^ ]+				{ mkL CmtWord }
<cmtSC>		[0-9]+"%"				{ mkL LitInteger }
<cmtSC>		"<code>"				{ mkL CmtCode `andBegin` codeSC }
<cmtSC>		"`"[^`]+"`"				{ mkL CmtCode }
<cmtSC>		"*/"					{ mkL CmtEnd `andBegin` 0 }
<cmtSC>		\n" "+"*/"				{ mkL CmtEnd `andBegin` 0 }
<cmtSC,codeSC>	\n" "+"*"				{ mkL PpNewline }
<cmtSC,codeSC>	\n					{ mkL PpNewline }
<cmtSC>		" "+					;

-- <code></code> blocks in comments.
<codeSC>	"</code>"				{ mkL CmtCode `andBegin` cmtSC }
<codeSC>	[^\<]+					{ mkL CmtCode }

-- Error handling.
<0,ppSC,cmtSC,codeSC>	.				{ mkL Error }

{
data Lexeme text = L AlexPosn LexemeClass text
    deriving (Show, Eq, Functor, Foldable, Traversable)

mkL :: Applicative m => LexemeClass -> AlexInput -> Int -> m (Lexeme String)
mkL c (p, _, _, str) len = pure $ L p c (take len str)

lexemePosn :: Lexeme text -> AlexPosn
lexemePosn (L p _ _) = p

lexemeClass :: Lexeme text -> LexemeClass
lexemeClass (L _ c _) = c

lexemeText :: Lexeme text -> text
lexemeText (L _ _ s) = s

lexemeLine :: Lexeme text -> Int
lexemeLine (L (AlexPn _ l _) _ _) = l

start :: Int -> AlexInput -> Int -> Alex (Lexeme String)
start code _ _ = do
    alexSetStartCode code
    alexMonadScan

alexEOF :: Alex (Lexeme String)
alexEOF = return (L (AlexPn 0 0 0) Eof "")

alexScanTokens :: String -> Either String [Lexeme String]
alexScanTokens str =
    runAlex str $ loop []
  where
    loop toks = do
        tok@(L _ cl _) <- alexMonadScan
        if cl == Eof
            then return $ reverse toks
            else loop $! (tok:toks)
}
