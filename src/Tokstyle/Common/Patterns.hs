{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Common.Patterns where

import           Data.Fix        (Fix (..))
import           Data.Text       (Text)
import           Language.Cimple (Lexeme (..), LexemeClass (..), Node,
                                  NodeF (..))

pattern TY_uint8_t, TY_uint16_t, TY_uint32_t, TY_uint64_t, TY_uint8_t_ptr :: Node (Lexeme Text)
pattern TY_uint8_t  <- Fix (TyStd (L _ IdStdType "uint8_t"))
pattern TY_uint16_t <- Fix (TyStd (L _ IdStdType "uint16_t"))
pattern TY_uint32_t <- Fix (TyStd (L _ IdStdType "uint32_t"))
pattern TY_uint64_t <- Fix (TyStd (L _ IdStdType "uint64_t"))

pattern TY_uint8_t_ptr <- Fix (TyPointer (Fix (TyStd (L _ IdStdType "uint8_t"))))
