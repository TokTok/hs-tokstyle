{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.C.Linter.Wraparound (descr) where

import           Control.Monad                 (forM_)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Language.C.Analysis.SemRep    (GlobalDecls)
import           Language.C.Analysis.TravMonad (Trav, getUserState)
import           Language.C.Data.Node          (NodeInfo, posOfNode)
import           Language.C.Data.Position      (isSourcePos)
import qualified Language.C.Pretty             as C
import           Language.C.Syntax.AST         (CAssignOp (..), CBinaryOp (..),
                                                CExpr, annotation)
import           Language.Cimple.Diagnostics   (DiagnosticSpan (..))
import           Prettyprinter                 (Doc, pretty, (<+>))
import           Prettyprinter.Render.Terminal (AnsiStyle)
import           Tokstyle.C.Analysis.Interval  (Facts, IntTy (..), Origin (..),
                                                Range (..), Term (..),
                                                TermKind (..), dependsOn, eval,
                                                solveFunction, subTerms)
import           Tokstyle.C.Env                (DiagnosticLevel (..), Env (..),
                                                posAndLen, recordLinterError,
                                                recordRichError)
import           Tokstyle.C.TravUtils          (backticks)


isComparison :: CBinaryOp -> Bool
isComparison = (`elem` [CEqOp, CNeqOp, CLeOp, CGrOp, CLeqOp, CGeqOp])

compareWith :: CBinaryOp -> Integer -> Integer -> Bool
compareWith = \case
    CEqOp  -> (==)
    CNeqOp -> (/=)
    CLeOp  -> (<)
    CGrOp  -> (>)
    CLeqOp -> (<=)
    _      -> (>=)

flipOp :: CBinaryOp -> CBinaryOp
flipOp = \case
    CLeOp  -> CGrOp
    CGrOp  -> CLeOp
    CLeqOp -> CGeqOp
    CGeqOp -> CLeqOp
    op     -> op

exprDoc :: CExpr -> Doc a
exprDoc = backticks . pretty . show . C.pretty

valuesDoc :: Range -> Doc a
valuesDoc (Range l h _)
    | l == h = pretty l
    | otherwise = "values from" <+> pretty l <+> "to" <+> pretty h

-- | The values that wrapped around, with the expression where they did.
wrappedIn :: Facts -> Term -> [(Range, NodeInfo)]
wrappedIn facts t = [ (r, site) | Just rs <- [eval facts t], r@(Range _ _ (Wrapped site)) <- rs ]

-- | Records an error at a term, pointing at where its value wrapped around if
-- that was elsewhere.
recordWrap :: Term -> NodeInfo -> Doc AnsiStyle -> Trav Env ()
recordWrap t site msg = recordRichError ni ErrorLevel msg spans []
  where
    ni = annotation (termExpr t)
    spans =
        [ DiagnosticSpan pos len ["wraps around here"]
        | posOfNode site /= posOfNode ni, isSourcePos (posOfNode site)
        , let (pos, len) = posAndLen site ]

-- | Warns if wrapping around makes one side of a comparison take values an
-- index reduced on the other side never has.
checkComparison :: Facts -> CBinaryOp -> Term -> Term -> Trav Env ()
checkComparison facts op wrapping other = case eval facts other of
    Just os@(_ : _) | all ((== Reduced) . origin) os -> do
        let oLo = minimum (map lo os)
            oHi = maximum (map hi os)
        case [ (w, site) | (w, site) <- wrappedIn facts wrapping, hi w < oLo || lo w > oHi ] of
            (w, site) : _ -> recordWrap wrapping site $
                exprDoc (termExpr wrapping) <+> "can wrap around to" <+> valuesDoc w
                <> ", but" <+> exprDoc (termExpr other) <+> "is always between"
                <+> pretty oLo <+> "and" <+> pretty oHi
                <> ", so the comparison is always"
                <+> (if compareWith op (lo w) oLo then "true" else "false")
                <+> "when it wraps"
            [] -> return ()
    _ -> return ()

-- | Warns if a value that wrapped around is reduced modulo a size that
-- doesn't divide the range it wrapped around.
checkRemainder :: Facts -> Term -> Term -> Trav Env ()
checkRemainder facts a b = case (eval facts b, termTy a, wrappedIn facts a) of
    (Just [Range n n' _], Just ty, (w, site) : _) | n == n', n > 0, (2 ^ tyBits ty) `mod` n /= 0 ->
        recordWrap a site $
            exprDoc (termExpr a) <+> "can wrap around to" <+> valuesDoc w
            <> ", and" <+> pretty (2 ^ tyBits ty :: Integer) <+> "is not a multiple of"
            <+> pretty n <> ", so" <+> backticks ("%" <+> pretty n)
            <+> "then gives a different index than it would without the wrap"
    _ -> return ()

-- | Warns if an index that wrapped around can be past the end of an array.
checkIndex :: Facts -> Term -> Term -> Integer -> Trav Env ()
checkIndex facts arr i len =
    case [ (w, site) | (w, site) <- wrappedIn facts i, hi w >= len ] of
        (w, site) : _ -> recordWrap i site $
            exprDoc (termExpr i) <+> "can wrap around to" <+> valuesDoc w
            <> ", past the end of" <+> exprDoc (termExpr arr)
            <> ", which has" <+> pretty len <+> "elements"
        [] -> return ()

-- | Warns if the difference of two narrow unsigned values, computed in @int@
-- after integer promotion, wraps around at the size of the wider unsigned type
-- it is converted to.
checkPromoted :: Facts -> Term -> Term -> Trav Env ()
checkPromoted facts converted inner = case (termTy converted, termKind inner, termTy inner) of
    (Just IntTy{tySigned = False, tyBits = bits}, TBin CSubOp x y, Just IntTy{tySigned = True})
        | Just w <- narrowest [x, y], w < bits
        , (r, _) : _ <- wrappedIn facts converted ->
            recordLinterError (annotation (termExpr inner)) $
                exprDoc (termExpr inner) <+> "can wrap around to" <+> valuesDoc r
                <> ", because it is the difference of" <+> pretty w <> "-bit values but wraps around at"
                <+> pretty bits <+> "bits here"
    _ -> return ()
  where
    narrowest ts = maximum <$> mapM width ts
    width t = case (termKind t, termTy t) of
        (TConst _, _)                                 -> Nothing
        (_, Just IntTy{tySigned = False, tyBits = b}) -> Just b
        _                                             -> Nothing

checkTerm :: Facts -> Term -> Trav Env ()
checkTerm facts t = case termKind t of
    TBin op l r | isComparison op -> do
        checkComparison facts op l r
        checkComparison facts (flipOp op) r l
    TBin CRmdOp a b -> checkRemainder facts a b
    TIndex arr i (Just len) -> checkIndex facts arr i len
    TCast a -> checkPromoted facts t a
    TAssign _ CAssignOp _ r -> checkPromoted facts t r
    _ -> return ()

analyse :: GlobalDecls -> Trav Env ()
analyse _ = do
    env <- getUserState
    forM_ (analysed env) $ \(fn, known) -> do
        let nodes = if dependsOn (program env) fn then solveFunction (program env) fn else known
        forM_ nodes $ \(_, facts, terms) -> mapM_ (checkTerm facts) (concatMap subTerms terms)


descr :: (GlobalDecls -> Trav Env (), (Text, Text))
descr = (analyse, ("wraparound", Text.unlines
    [ "Checks for integer arithmetic that wraps around where it is used as an index:"
    , "compared to an index reduced with `%` or `&`, reduced with `%` by a size that"
    , "doesn't divide the type's range, or used to index an array it can be past the"
    , "end of. Also checks for the difference of two 16-bit values that wraps around"
    , "at 32 bits because integer promotion computes it in `int`."
    , ""
    , "The checks follow values through locals and fields, take the conditions that"
    , "guard them into account, and know which values the program stores in struct"
    , "fields, returns from functions, and passes to static functions."
    , ""
    , "**Reason:** in a ring buffer of size `N`, `(uint16_t)(start - 1)` wraps to"
    , "65535 when `start` is 0, not to `N - 1`. Comparing it to `id % N` then never"
    , "matches. Use `(start + N - 1) % N` instead."
    ]))
