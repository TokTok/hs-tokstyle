{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE TupleSections     #-}
module Tokstyle.Linter.TaggedUnion (descr) where

import           Control.Monad               (forM_, when)
import           Control.Monad.State.Strict  (State, execState, get, modify,
                                              put)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..), foldFixM, unFix)
import           Data.List                   (elemIndex, find, isPrefixOf,
                                              sortBy)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (isJust, mapMaybe)
import           Data.Ord                    (comparing)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Debug.Trace                 (traceM)
import qualified Debug.Trace                 as Debug
import           Language.Cimple             (BinaryOp (..), Lexeme (..),
                                              LiteralType (..), Node,
                                              NodeF (..), lexemeText)
import qualified Language.Cimple             as C
import           Language.Cimple.Diagnostics (HasDiagnostics (..), warn)
import           Language.Cimple.TraverseAst (AstActions (..), astActions,
                                              doNode, traverseAst)
import           Text.Read                   (readMaybe)
import qualified Tokstyle.Common             as Common
import           Tokstyle.Common.TypeSystem  (StdType (..), TypeDescr (..),
                                              TypeInfo (..), TypeRef (..),
                                              TypeSystem, collect, collectTypes,
                                              getTypeRefName, lookupType)


data LinterState = LinterState
    { guards   :: [(Node (C.Lexeme Text), Text, [Text])]
    , warnings :: [Text]
    , varTypes :: Map.Map Text Text
    }

instance HasDiagnostics LinterState where
    addDiagnostic w st =
        dtrace ("Adding diagnostic: " ++ Text.unpack w) $
        st { warnings = w : warnings st }

debugging :: Bool
debugging = False

dtraceM :: String -> State LinterState ()
dtraceM msg = when debugging (Debug.traceM msg)

dtrace :: String -> a -> a
dtrace msg = if debugging then Debug.trace msg else id


isVoidPtr :: (Lexeme Text, TypeInfo) -> Bool
isVoidPtr (_, ty) = isVoid ty
  where
    isVoid (Pointer (BuiltinType VoidTy)) = True
    isVoid (Nullable t)                   = isVoid t
    isVoid (Nonnull t)                    = isVoid t
    isVoid (Owner t)                      = isVoid t
    isVoid (Const t)                      = isVoid t
    isVoid _                              = False


typeSize :: TypeSystem -> TypeInfo -> Maybe Int
typeSize ts = \case
    BuiltinType BoolTy  -> Just 1
    BuiltinType CharTy  -> Just 1
    BuiltinType U08Ty   -> Just 1
    BuiltinType S08Ty   -> Just 1
    BuiltinType U16Ty   -> Just 2
    BuiltinType S16Ty   -> Just 2
    BuiltinType U32Ty   -> Just 4
    BuiltinType S32Ty   -> Just 4
    BuiltinType U64Ty   -> Just 8
    BuiltinType S64Ty   -> Just 8
    BuiltinType SizeTy  -> Just 8
    BuiltinType F32Ty   -> Just 4
    BuiltinType F64Ty   -> Just 8
    Pointer _           -> Just 8
    Owner t             -> typeSize ts t
    Nonnull t           -> typeSize ts t
    Nullable t          -> typeSize ts t
    Const t             -> typeSize ts t
    Sized t _           -> typeSize ts t
    Array (Just t) dims -> (*) <$> typeSize ts t <*> productOfDims dims
    TypeRef _ (L _ _ n) -> lookupTypeSize n
    _                   -> Nothing
  where
    productOfDims dims = product <$> mapM getDim dims
    getDim (IntLit (L _ _ val)) = readMaybe (Text.unpack val)
    getDim _                    = Nothing

    lookupTypeSize name = case lookupType name ts of
        Just (StructDescr _ members) -> sum <$> mapM (typeSize ts . snd) members
        Just (UnionDescr _ members)  -> maximum <$> mapM (typeSize ts . snd) members
        Just (EnumDescr _ _)         -> Just 4
        Just (IntDescr _ _)          -> Just 4
        Just (AliasDescr _ target)   -> typeSize ts target
        _                            -> Nothing


isCompatible :: TypeSystem -> [(Lexeme Text, TypeInfo)] -> Bool
isCompatible ts members =
    case mapMaybe (typeSize ts . snd) members of
        sizes | length sizes == length members && not (null sizes) ->
            all (== head sizes) sizes
        _ -> False


isBuiltin :: TypeInfo -> Bool
isBuiltin (BuiltinType _)    = True
isBuiltin (Const t)          = isBuiltin t
isBuiltin (Nonnull t)        = isBuiltin t
isBuiltin (Nullable t)       = isBuiltin t
isBuiltin (Sized t _)        = isBuiltin t
isBuiltin (Array (Just t) _) = isBuiltin t
isBuiltin (Array Nothing _)  = True -- Array of self? Should not happen in this context.
isBuiltin _                  = False


needsTagging :: TypeSystem -> TypeDescr -> Bool
needsTagging ts desc@(UnionDescr (L _ _ uname) members)
    | uname == "IP_Union" = False
    | all (isBuiltin . snd) members && isCompatible ts members = False
    | otherwise = True
needsTagging _ _ = False


isCorrectTag :: Text -> Text -> Text -> Bool
isCorrectTag prefix mText tagValue =
    let -- Normalize by converting to uppercase and splitting into words
        toWords name = filter (not . Text.null) $ Text.splitOn "_" $ Text.toUpper name

        mWords = toWords mText
        vFullWords = toWords tagValue
        vPrefixWords = toWords prefix

        -- Strip prefix words from the beginning of tag words
        vWords = if vPrefixWords `isPrefixOf` vFullWords
                 then drop (length vPrefixWords) vFullWords
                 else vFullWords

        -- Principled matching: m's words must appear as a contiguous sub-sequence in v's words.
        -- E.g. 'friend_message' matches 'TOX_EVENT_FRIEND_MESSAGE'
        -- 'ptr' matches 'TAG_PTR'
        -- 'u32' matches 'UINT32' (we handle this as a single word match)

        isSubSeq [] _ = True
        isSubSeq _ [] = False
        isSubSeq sub@(s:ss) (x:xs)
            | s == x || matchAbbrev s x = ss `isPrefixOf` xs || isSubSeq sub xs
            | otherwise = isSubSeq sub xs

        matchAbbrev m v =
            let normalize n = Text.filter (`notElem` ("AEIOU" :: String)) n
            in normalize m == normalize v
               || m `Text.isPrefixOf` v
               || v `Text.isPrefixOf` m
               || m `Text.isSuffixOf` v
               || v `Text.isSuffixOf` m

    in isSubSeq mWords vWords


-- | Finds all (StructName, UnionMemberName, TagMemberName, UnionTypeName, EnumTypeName, Prefix, [Errors])
findTaggedUnions :: TypeSystem -> [(Text, Text, Text, Text, Text, Text, [Text])]
findTaggedUnions ts = concatMap findInStruct (Map.elems ts)
  where
    findInStruct (StructDescr (L _ _ sname) members) =
        dtrace ("Checking struct " ++ Text.unpack sname) $
        let unions = filter (isInterestingUnion . snd) members
        in dtrace ("Found unions: " ++ show (map (lexemeText . fst) unions)) $
           concatMap (findTag sname members) unions
    findInStruct _ = []

    isInterestingUnion (TypeRef UnionRef (L _ _ uname)) =
        case Map.lookup (Text.toLower uname) ts of
            Just desc -> needsTagging ts desc
            Nothing   -> False
    isInterestingUnion _ = False

    findTag sname members (L _ _ mname, TypeRef UnionRef (L _ _ uname)) =
        let (before, _) = break ((== mname) . lexemeText . fst) members
            tags = filter (isPotentialTag . snd) before
        in dtrace ("findTag: sname=" ++ Text.unpack sname ++ " mname=" ++ Text.unpack mname ++ " uname=" ++ Text.unpack uname ++ " tags=" ++ show (map (lexemeText . fst) tags)) $
           case tags of
            ((L _ _ tname, tagTy) : _) ->
                dtrace ("  found potential tag " ++ Text.unpack tname) $
                case getEnumName tagTy of
                    Just ename ->
                        case checkOrder ename uname of
                            Just (prefix, errs) -> [(sname, mname, tname, uname, ename, prefix, errs)]
                            Nothing             -> dtrace "  checkOrder failed" []
                    Nothing -> dtrace "  not an enum type" []
            _ -> dtrace "  no potential tags found before union" []
    findTag _ _ _ = []

    getEnumName (TypeRef EnumRef (L _ _ n)) = Just n
    getEnumName _                           = Nothing

    isPotentialTag (TypeRef EnumRef _) = True
    isPotentialTag _                   = False

    longestCommonPrefix [] = ""
    longestCommonPrefix (x:xs) = foldl commonPrefix x xs
      where
        commonPrefix a b = Text.pack $ map fst $ takeWhile (uncurry (==)) $ zip (Text.unpack a) (Text.unpack b)

    checkOrder ename uname =
        case (Map.lookup (Text.toLower ename) ts, Map.lookup (Text.toLower uname) ts) of
            (Just (EnumDescr _ enumMembers), Just (UnionDescr _ unionMembers)) ->
                let enumLexemes = mapMaybe getEnumLexeme enumMembers
                    getEnumLexeme (EnumMem l) = Just $ lexemeText l
                    getEnumLexeme _           = Nothing

                    prefix = longestCommonPrefix enumLexemes

                    findMatch n = find (isCorrectTag prefix n) enumLexemes

                    unionMatches = map (\(l@(L _ _ n), _) -> (l, findMatch n)) unionMembers
                    matched = filter (isJust . snd) unionMatches

                    -- Heuristic: if at least some members match, it's likely the tag.
                    isLikelyTag = not (null matched)
                in if not isLikelyTag
                   then Nothing
                   else
                       let missingMatches = [ n | (L _ _ n, Nothing) <- unionMatches ]
                           errs0 = [ "union member `" <> n <> "` does not have a matching enum member in `" <> ename <> "`" | n <- missingMatches ]

                           voidPtrs = [ lexemeText n | m@(n, _) <- unionMembers, isVoidPtr m ]
                           errs1 = [ "union `" <> uname <> "` contains a void pointer: `" <> v <> "`" | v <- voidPtrs ]

                           matchedIndices = mapMaybe (\(l, m) -> (l,) <$> (m >>= (`elemIndex` enumLexemes))) unionMatches

                           isSorted [] = True
                           isSorted [_] = True
                           isSorted ((_, i1):rest@((_, i2):_)) = i1 <= i2 && isSorted rest

                           errs2 = if isSorted matchedIndices
                                   then []
                                   else let expected = map fst $ sortBy (comparing snd) matchedIndices
                                        in [ "order of members in union `" <> uname <> "` should be changed to `" <> Text.intercalate ", " (map lexemeText expected) <> "` to match enum `" <> ename <> "`" ]
                       in Just (prefix, errs0 ++ errs1 ++ errs2)
            _ -> Nothing


-- | Finds unions with pointers that are NOT used in a tagged way in any struct.
findUntaggedUnions :: TypeSystem -> [(Text, Lexeme Text)]
findUntaggedUnions ts = mapMaybe check (Map.elems ts)
  where
    check desc@(UnionDescr name@(L _ _ uname) _)
        | needsTagging ts desc && not (isUsedAsTagged uname) = Just (uname, name)
    check _ = Nothing

    isUsedAsTagged uname = uname `elem` map (\(_, _, _, u, _, _, _) -> u) (findTaggedUnions ts)



linter :: TypeSystem -> [(Text, Text, Text, Text, Text, Text, [Text])] -> [Text] -> AstActions (State LinterState) Text
linter ts tagged untagged = actions
  where
    withBoolCtx :: State LinterState a -> State LinterState a
    withBoolCtx act = act

    getTypeRefNameFromNode n =
        let tys = State.evalState (foldFixM collectTypes n) ts
        in case tys of
            (t:_) -> getTypeRefName t
            _     -> Nothing

    getVarDecl (Fix (C.VarDecl ty (L _ _ name) _)) = (name,) <$> getTypeRefNameFromNode ty
    getVarDecl _                                   = Nothing

    getTypeOf vt (Fix node) = case node of
        C.VarExpr (L _ _ v) -> Map.lookup v vt
        C.MemberAccess e m -> do
            tName <- getTypeOf vt e
            lookupMemberType tName (lexemeText m)
        C.PointerAccess e m -> do
            tName <- getTypeOf vt e
            lookupMemberType tName (lexemeText m)
        C.ParenExpr e -> getTypeOf vt e
        C.CastExpr ty _ -> getTypeRefNameFromNode ty
        _ -> Nothing

    lookupMemberType tName mName =
        case lookupType tName ts of
            Just (StructDescr _ members) -> findMember mName members
            Just (UnionDescr _ members)  -> findMember mName members
            _                            -> Nothing

    findMember mName members =
        case filter ((== mName) . lexemeText . fst) members of
            ((_, ty):_) -> getTypeRefName ty
            _           -> Nothing

    actions = astActions
        { doNode = \file node act ->
            case unFix node of
                C.Struct (L _ _ sname) _ -> do
                    let matches = filter (\(sn, _, _, _, _, _, _) -> sn == sname) tagged
                    forM_ matches $ \(_, _, _, _, _, _, errs) ->
                        forM_ errs $ \err -> warn file node err
                    act

                C.Union (L _ _ uname) _ | uname `elem` untagged -> do
                    warn file node $ "union `" <> uname <> "` must be tagged in a struct"
                    act

                C.IfStmt cond trueBody maybeFalseBody -> do
                    newGuards <- collectGuards' cond
                    oldGuards <- guards <$> get
                    withBoolCtx $ traverseAst actions { doNode = \_ -> doNode actions file } cond
                    modify $ \s -> s { guards = newGuards ++ oldGuards }
                    withBoolCtx $ traverseAst actions { doNode = \_ -> doNode actions file } trueBody

                    if alwaysExits trueBody
                    then do
                        negGuards <- collectNegativeGuards' cond
                        modify $ \s -> s { guards = negGuards ++ oldGuards }
                    else do
                        modify $ \s -> s { guards = oldGuards }

                    case maybeFalseBody of
                        Just falseBody -> do
                            negGuards <- collectNegativeGuards' cond
                            modify $ \s -> s { guards = negGuards ++ oldGuards }
                            withBoolCtx $ traverseAst actions { doNode = \_ -> doNode actions file } falseBody
                            if alwaysExits falseBody
                            then do
                                -- If false branch exits, then true branch's guards must hold for the rest of the function.
                                -- This is already handled by adding newGuards before, but we need to keep them.
                                modify $ \s -> s { guards = newGuards ++ oldGuards }
                            else do
                                modify $ \s -> s { guards = oldGuards }
                        Nothing -> return ()

                C.WhileStmt cond body -> do
                    newGuards <- collectGuards' cond
                    oldGuards <- guards <$> get
                    withBoolCtx $ traverseAst actions { doNode = \_ -> doNode actions file } cond
                    modify $ \s -> s { guards = newGuards ++ oldGuards }
                    withBoolCtx $ traverseAst actions { doNode = \_ -> doNode actions file } body
                    modify $ \s -> s { guards = oldGuards }

                C.DoWhileStmt body cond -> do
                    withBoolCtx $ traverseAst actions { doNode = \_ -> doNode actions file } body
                    withBoolCtx $ traverseAst actions { doNode = \_ -> doNode actions file } cond

                C.ForStmt init_ cond incr body -> do
                    withBoolCtx $ traverseAst actions { doNode = \_ -> doNode actions file } init_
                    newGuards <- collectGuards' cond
                    oldGuards <- guards <$> get
                    withBoolCtx $ traverseAst actions { doNode = \_ -> doNode actions file } cond
                    modify $ \s -> s { guards = newGuards ++ oldGuards }
                    withBoolCtx $ traverseAst actions { doNode = \_ -> doNode actions file } incr
                    withBoolCtx $ traverseAst actions { doNode = \_ -> doNode actions file } body
                    modify $ \s -> s { guards = oldGuards }

                C.SwitchStmt expr body -> do
                    -- For a switch, we track the expression being switched on.
                    -- Individual 'case' labels will add the specific tag value.
                    let actions' = actionsForSwitch expr
                    withBoolCtx $ traverseAst actions' { doNode = \_ -> doNode actions' file } body

                C.TernaryExpr cond trueExpr falseExpr -> do
                    newGuards <- collectGuards' cond
                    oldGuards <- guards <$> get
                    withBoolCtx $ traverseAst actions { doNode = \_ -> doNode actions file } cond
                    modify $ \s -> s { guards = newGuards ++ oldGuards }
                    withBoolCtx $ traverseAst actions { doNode = \_ -> doNode actions file } trueExpr
                    modify $ \s -> s { guards = oldGuards }
                    withBoolCtx $ traverseAst actions { doNode = \_ -> doNode actions file } falseExpr
                    modify $ \s -> s { guards = oldGuards }

                C.BinaryExpr lhs op rhs -> do
                    withBoolCtx $ do
                        case op of
                            C.BopAnd -> do
                                traverseAst actions { doNode = \_ -> doNode actions file } lhs
                                newGuards <- collectGuards' lhs
                                oldGuards <- guards <$> get
                                modify $ \s -> s { guards = newGuards ++ oldGuards }
                                traverseAst actions { doNode = \_ -> doNode actions file } rhs
                                modify $ \s -> s { guards = oldGuards }
                            _ -> do
                                traverseAst actions { doNode = \_ -> doNode actions file } lhs
                                traverseAst actions { doNode = \_ -> doNode actions file } rhs

                C.UnaryExpr C.UopNot e -> withBoolCtx $ traverseAst actions { doNode = \_ -> doNode actions file } e

                C.MemberAccess expr mname -> do
                    checkAccess file expr mname
                    act

                C.PointerAccess expr mname -> do
                    checkAccess file expr mname
                    act

                C.FunctionDefn _ proto body -> do
                    case unFix proto of
                        C.FunctionPrototype _ _ params -> do
                            let newVars = Map.fromList $ mapMaybe getVarDecl params
                            st <- get
                            let oldVars = varTypes st
                            put st { guards = [], varTypes = newVars `Map.union` oldVars }
                            traverseAst actions { doNode = \_ -> doNode actions file } body
                            modify $ \s -> s { guards = guards st, varTypes = oldVars }
                        _ -> act

                C.VarDecl ty (L _ _ name) _ -> do
                    case getTypeRefNameFromNode ty of
                        Just t -> modify $ \s -> s { varTypes = Map.insert name t (varTypes s) }
                        Nothing -> return ()
                    act

                C.AssignExpr lhs C.AopEq rhs -> do
                    case extractGuards' lhs of
                        [(structExpr, tname)] -> do
                            let cExpr = canonical structExpr
                            modify $ \s -> s { guards = filter (\(gExpr, gTname, _) -> gTname /= tname || canonical gExpr /= cExpr) (guards s) }
                            case extractConstants rhs of
                                [tagValue] -> modify $ \s -> s { guards = (structExpr, tname, [tagValue]) : guards s }
                                _          -> return ()
                        _ -> return ()
                    withBoolCtx $ traverseAst actions { doNode = \_ -> doNode actions file } lhs
                    withBoolCtx $ traverseAst actions { doNode = \_ -> doNode actions file } rhs

                C.Return{} -> withBoolCtx act
                C.FunctionCall{} -> withBoolCtx act

                _ -> act
        }

    actionsForSwitch switchedExpr = actions
        { doNode = \file node act ->
            case unFix node of
                C.Case val body -> do
                    case unFix val of
                        C.LiteralExpr ConstId tagValue -> do
                            let newGuards = [ (expr, tname, [lexemeText tagValue]) | (expr, tname) <- extractGuards' switchedExpr ]
                            oldGuards <- guards <$> get
                            modify $ \s -> s { guards = newGuards ++ oldGuards }
                            traverseAst actions { doNode = \_ -> doNode actions file } body
                            modify $ \s -> s { guards = oldGuards }
                        _ -> act
                _ -> doNode actions file node act
        }

    collectGuards' (Fix (C.BinaryExpr lhs BopEq rhs)) = do
        let l = extractGuardsFromBin lhs rhs
        let r = extractGuardsFromBin rhs lhs
        l_nested <- collectGuards' lhs
        r_nested <- collectGuards' rhs
        return (l ++ r ++ l_nested ++ r_nested)
    collectGuards' (Fix (C.ParenExpr e)) = collectGuards' e
    collectGuards' (Fix (C.BinaryExpr lhs BopAnd rhs)) = do
        l <- collectGuards' lhs
        r <- collectGuards' rhs
        return (l ++ r)
    collectGuards' (Fix (C.BinaryExpr lhs BopOr rhs)) = do
        l <- collectGuards' lhs
        r <- collectGuards' rhs
        let r_facts = [ ((canonical e, t), v) | (e, t, v) <- r ]
        return $ mapMaybe (\(e, t, v) -> (e, t,) . (v ++) <$> lookup (canonical e, t) r_facts) l
    collectGuards' _ = return []

    collectNegativeGuards' (Fix (C.BinaryExpr lhs BopNe rhs)) = do
        let l = extractGuardsFromBin lhs rhs
        let r = extractGuardsFromBin rhs lhs
        return (l ++ r)
    collectNegativeGuards' (Fix (C.ParenExpr e)) = collectNegativeGuards' e
    collectNegativeGuards' (Fix (C.BinaryExpr lhs BopAnd rhs)) = do
        -- For AND, we only know that it's NOT true if BOTH are false? No.
        -- If !(A && B), we don't know much.
        return []
    collectNegativeGuards' (Fix (C.BinaryExpr lhs BopOr rhs)) = do
        -- For OR, if !(A || B), then !A AND !B.
        l <- collectNegativeGuards' lhs
        r <- collectNegativeGuards' rhs
        return (l ++ r)
    collectNegativeGuards' _ = return []

    alwaysExits (Fix (C.Return _))            = True
    alwaysExits (Fix (C.Break))               = True
    alwaysExits (Fix (C.Continue))            = True
    alwaysExits (Fix (C.CompoundStmt stmts))  = any alwaysExits stmts
    alwaysExits (Fix (C.IfStmt _ t (Just f))) = alwaysExits t && alwaysExits f
    alwaysExits _                             = False

    extractGuardsFromBin lhs rhs =
        [ (expr, tname, [tagValue]) | (expr, tname) <- extractGuards' lhs, tagValue <- extractConstants rhs ]

    extractConstants (Fix (C.LiteralExpr ConstId val)) = [lexemeText val]
    extractConstants (Fix (C.ParenExpr e))             = extractConstants e
    extractConstants _                                 = []

    extractGuards' (Fix (C.MemberAccess expr tname))  = [(expr, lexemeText tname)]
    extractGuards' (Fix (C.PointerAccess expr tname)) = [(expr, lexemeText tname)]
    extractGuards' (Fix (C.ParenExpr e))              = extractGuards' e
    extractGuards' _                                  = []

    checkAccess file expr mname = do
        let mText = lexemeText mname
        dtraceM $ "checkAccess for member " ++ Text.unpack mText ++ " in expression " ++ show expr
        case extractUnionMember expr of
            Just (structExpr, uName) -> do
                dtraceM $ "Found union member candidate: " ++ Text.unpack uName ++ " with structExpr " ++ show structExpr
                st <- get
                let tName = getTypeOf (varTypes st) structExpr
                dtraceM $ "Type of structExpr: " ++ show tName
                case tName of
                    Just tn -> do
                        let matches = filter (\(sn, um, _, _, _, _, _) -> sn == tn && um == uName) tagged
                        dtraceM $ "Matches: " ++ show matches
                        forM_ matches $ \(_, _, tname, _, _, prefix, _) -> do
                            let checkedTags = findCheckedTags structExpr tname (guards st)
                            dtraceM $ "Checking tag " ++ Text.unpack tname ++ " against checkedTags " ++ show checkedTags
                            if null checkedTags
                            then do
                                dtraceM "No check found"
                                warn file mname $ "access to union member `" <> mText <> "` is not guarded by a check on `" <> tname <> "`"
                            else do
                                let incorrect = filter (not . all (isCorrectTag prefix mText)) checkedTags
                                when (not $ null incorrect) $ do
                                    dtraceM $ "WARNING: Incorrect tag(s): " ++ show incorrect
                                    warn file mname $ "access to union member `" <> mText <> "` is not guarded by a check on `" <> expectedTag mText <> "`"
                    Nothing -> dtraceM $ "Unknown struct type for member access: " ++ Text.unpack uName
            Nothing -> dtraceM "Not a union member access"

    extractUnionMember (Fix (C.MemberAccess expr uName)) =
        case unFix expr of
            C.MemberAccess structExpr _  -> Just (structExpr, lexemeText uName)
            C.PointerAccess structExpr _ -> Just (structExpr, lexemeText uName)
            _                            -> Just (expr, lexemeText uName)
    extractUnionMember (Fix (C.PointerAccess expr uName)) =
        case unFix expr of
            C.MemberAccess structExpr _  -> Just (structExpr, lexemeText uName)
            C.PointerAccess structExpr _ -> Just (structExpr, lexemeText uName)
            _                            -> Just (expr, lexemeText uName)
    extractUnionMember _ = Nothing

    findCheckedTags structExpr tname gs =
        let cExpr = canonical structExpr
        in dtrace ("findCheckedTags: structExpr=" ++ show cExpr ++ " tname=" ++ Text.unpack tname) $
           map (\(_, _, vals) -> vals) $ filter (\(gExpr, gTname, _) -> gTname == tname && canonical gExpr == cExpr) gs

    canonical :: Node (C.Lexeme Text) -> Node (C.Lexeme Text)
    canonical (Fix (C.ParenExpr e)) = canonical e
    canonical (Fix (C.VarExpr (L _ _ v))) = Fix (C.VarExpr (L (C.AlexPn 0 0 0) C.IdVar v))
    canonical (Fix (C.MemberAccess e (L _ _ m))) = Fix (C.MemberAccess (canonical e) (L (C.AlexPn 0 0 0) C.IdVar m))
    canonical (Fix (C.PointerAccess e (L _ _ m))) = Fix (C.PointerAccess (canonical e) (L (C.AlexPn 0 0 0) C.IdVar m))
    canonical (Fix n) = Fix (fmap canonical n)

    expectedTag mText = "TAG_" <> Text.toUpper mText -- Heuristic for error message



analyse :: [(FilePath, [Node (C.Lexeme Text)])] -> [Text]
analyse sources =
    let ts = collect sources
        tagged = findTaggedUnions ts
        untaggedNames = map fst $ findUntaggedUnions ts

        linterM = forM_ sources $ \tu -> traverseAst (linter ts tagged untaggedNames) (Common.skip ["third_party/cmp/cmp.h"] tu)

        finalState = execState linterM (LinterState [] [] Map.empty)
    in reverse (warnings finalState)

descr :: ([(FilePath, [Node (C.Lexeme Text)])] -> [Text], (Text, Text))
descr = (analyse, ("tagged-union", Text.unlines
    [ "Checks that all unions with incompatible types (pointers) in them are tagged,"
    , "and when accessing their members, the tag is checked."
    ]))
