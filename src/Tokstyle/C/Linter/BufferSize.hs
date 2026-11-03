{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.C.Linter.BufferSize (descr, looksAt) where

import           Control.Monad                 (foldM, forM_)
import           Data.List                     (inits, sortOn, tails)
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (isJust)
import qualified Data.Set                      as Set
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Language.C.Analysis.SemRep    (ArraySize (..), FunDef (..),
                                                GlobalDecls, IdentDecl (..),
                                                ObjDef (..), Type (..),
                                                VarDecl (..), VarName (..))
import           Language.C.Analysis.TravMonad (Trav, getUserState)
import           Language.C.Analysis.TypeUtils (canonicalType)
import           Language.C.Data.Ident         (identToString)
import           Language.C.Data.Node          (nameOfNode, posOfNode)
import           Language.C.Syntax.AST
import           Prettyprinter                 (Doc, pretty, (<+>))
import           Prettyprinter.Render.Terminal (AnsiStyle)
import           Tokstyle.C.Analysis.Access    (Access (..), AccessBy (..),
                                                accesses, changedNames)
import           Tokstyle.C.Analysis.Interval  (Facts, IntTy (..), Range (..),
                                                Term (..), TermKind (..),
                                                atMost, constantSum, eval,
                                                exactSum, intType, subTerms,
                                                times, wrapSite)
import           Tokstyle.C.Analysis.Known     (allocators)
import           Tokstyle.C.Analysis.Syntax    (exprChildren, localDecls)
import           Tokstyle.C.Env                (Env (..))
import           Tokstyle.C.IntervalLinter     (analysedFunctions, exprDoc,
                                                recordWrap, valuesDoc)
import           Tokstyle.C.TravUtils          (backticks)


-- | The numbers whose product is the size in bytes of the memory an
-- allocation returns.
allocated :: CExpr -> Maybe [CExpr]
allocated = \case
    CCast _ e _ -> allocated e
    CCall (CVar f _) args _
        | Just (arity, factors) <- Map.lookup (identToString f) allocators, length args == arity ->
            Just (map (args !!) factors)
    _ -> Nothing

-- | The functions the check looks at: those that declare an array whose size
-- isn't a constant, or allocate memory.
looksAt :: FunDef -> Bool
looksAt (FunDef _ body _) = any sized (localDecls body)
  where
    sized = \case
        CDecl _ ds _ -> or
            [ any variable derived || maybe False allocates ini
            | (Just (CDeclr _ derived _ _ _), ini, _) <- ds ]
        _ -> False
    variable = \case
        CArrDeclr _ (CArrSize _ CConst{}) _ -> False
        CArrDeclr _ (CArrSize _ _) _        -> True
        _                                   -> False
    allocates = \case
        CInitExpr e _ -> isJust (allocated e)
        _             -> False

-- | The byte buffers among a function's locals that always point at the
-- memory they were made with, the expressions whose product gives their
-- sizes, and whether those expressions keep their values.
buffers :: FunDef -> [IdentDecl] -> [(String, ([CExpr], Bool))]
buffers (FunDef _ body _) locals =
    [ (name, (size, all (`Set.notMember` changed) (concatMap names size)))
    | ObjectDef (ObjDef (VarDecl (VarName i _) _ ty) ini _) <- locals
    , let name = identToString i
    , name `Set.notMember` changed
    , Just size <- [sizeOf (canonicalType ty) ini]
    ]
  where
    sizeOf ty ini = case ty of
        ArrayType el (ArraySize _ e) _ _ | isByte el            -> Just [e]
        PtrType el _ _ | isByte el, Just (CInitExpr e _) <- ini -> allocated e
        _                                                       -> Nothing
    isByte el = maybe False ((== 8) . tyBits) (intType el)

    changed = changedNames body
    names e = [ identToString i | CVar i _ <- everyExpr e ]
    everyExpr e = e : concatMap everyExpr (exprChildren e)

-- | How many bytes into its buffer an access can reach.
reach :: Facts -> Access -> Maybe Integer
reach facts acc = (+ accConst acc) . sum <$> mapM furthest (accTerms acc)
  where
    furthest (k, x) = eval facts x >>= \case
        [] -> Nothing
        rs -> Just (maximum [ k * v | r <- rs, v <- [lo r, hi r] ])

-- | A buffer's size in bytes: the numbers whose product it is, with the
-- values each has when the buffer is made, and whether they keep them.
data Size = Size
    { sizeFactors :: [(Term, [Range])]
    , sizeKept    :: Bool
    }

fits :: Facts -> Access -> Size -> Bool
fits facts acc size = maybe False (atMost facts (accTerms acc) (accConst acc)) $
    mapM (exactSum facts . fst) (sizeFactors size) >>= foldM times (constantSum 1)

-- | A size without its factor of one, like @sizeof(uint8_t)@.
inBytes :: Term -> Term
inBytes t = case termKind t of
    TBin CMulOp a b | TConst 1 <- termKind b -> inBytes a
    _                                        -> t

bytesDoc :: Integer -> Doc a
bytesDoc 1 = "1 byte"
bytesDoc n = pretty n <+> "bytes"

analyse :: GlobalDecls -> Trav Env ()
analyse _ = do
    env <- getUserState
    functions <- analysedFunctions looksAt
    forM_ functions $ \(fd@(FunDef (VarDecl self _ _) _ _), nodes) -> do
        let locals = case self of
                VarName i _ -> Map.findWithDefault [] (identToString i) (localsOf env)
                NoName      -> []
            bufs = Map.fromList (buffers fd locals)
            sizeNames = Map.fromList [ (n, b) | (b, (es, _)) <- Map.toList bufs, e <- es, Just n <- [nameOfNode (annotation e)] ]
            factors = Map.fromListWith Map.union
                [ (b, Map.singleton n (inBytes s, rs))
                | (_, facts, terms) <- nodes, s <- concatMap subTerms terms
                , Just n <- [nameOfNode (annotation (termExpr s))]
                , Just b <- [Map.lookup n sizeNames]
                , Just rs <- [eval facts (inBytes s)] ]
            sizes = Map.fromList
                [ (b, Size found kept)
                | (b, (es, kept)) <- Map.toList bufs
                , let found = Map.elems (Map.findWithDefault Map.empty b factors)
                , length found == length es ]
            problems =
                [ (posOfNode (annotation (termExpr t)), accBuffer acc, report t acc (termExpr factor) r site end)
                | (_, facts, terms) <- nodes, t <- concatMap subTerms terms, acc <- accesses (program env) t
                , Just size <- [Map.lookup (accBuffer acc) sizes]
                , not (sizeKept size && fits facts acc size)
                , Just end <- [reach facts acc]
                , (factor, r, site) : _ <- [wrapped (sizeFactors size) end] ]
        -- One error for each buffer, at the first access past its size.
        sequence_ . Map.elems $ Map.fromListWith (\_ old -> old)
            [ (b, act) | (_, b, act) <- sortOn (\(pos, _, _) -> pos) problems ]
  where
    -- A factor's values that wrapped around, with which the size can be
    -- less than an access reaches.
    wrapped factors end =
        [ (factor, r, site)
        | (before, (factor, rs) : after) <- zip (inits factors) (tails factors)
        , Just others <- [mapM (least . snd) (before ++ after)]
        , r <- rs, Just site <- [wrapSite (origin r)], lo r * product others < end ]
    least = \case
        [] -> Nothing
        rs -> Just (minimum (map lo rs))
    report t acc size r site end = recordWrap t site $
        exprDoc size <+> "can wrap around to" <+> valuesDoc r
        <> ", so" <+> backticks (pretty (accBuffer acc)) <+> "can be smaller than the"
        <+> bytesDoc end <+> byDoc (accBy acc) <+> "reaches"
    byDoc = \case
        ByElement e -> exprDoc e
        ByCall fn   -> backticks (pretty fn)


descr :: (GlobalDecls -> Trav Env (), (Text, Text))
descr = (analyse, ("buffer-size", Text.unlines
    [ "Checks for local byte buffers whose size can wrap around and that are then"
    , "accessed past that size, by indexing or by a call that reads or writes them,"
    , "such as `memcpy` or a function that accesses its buffer argument up to another"
    , "argument."
    , ""
    , "**Reason:** `const uint16_t size = length + HEADER_SIZE;` truncates to a small"
    , "value when `length` is close to 65536, and copying `length` bytes into a"
    , "buffer of that size writes past its end. Compute sizes in a type that can hold"
    , "them, and limit them before making the buffer."
    ]))
