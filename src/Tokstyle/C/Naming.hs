{-# LANGUAGE LambdaCase #-}
module Tokstyle.C.Naming where

import qualified Data.Char                  as Char
import qualified Data.List                  as List
import           Language.C
import           Language.C.Analysis.SemRep
import           Language.C.Data.Ident
import           System.FilePath


getSrcFile :: NodeInfo -> String
getSrcFile (OnlyPos  _ (pos, _)  ) = posFile pos
getSrcFile (NodeInfo _ (pos, _) _) = posFile pos


toLower :: String -> String
toLower = map Char.toLower


check :: CTranslUnit -> GlobalDecls -> [CError]
check (CTranslUnit edecls ni) _ =
    reverse . map (CError . mkNamingError) . foldl globalNamesCED [] $ edecls
  where
    srcFile = getSrcFile ni
    namespace =
        case toLower . takeFileName . dropExtension $ srcFile of
            "audio"          -> "ac"
            "bwcontroller"   -> "bwc"
            "list"           -> "bs_list"
            "messenger"      -> "m"
            "network"        -> "net"
            "onion_announce" -> "onion"
            "onion_client"   -> "onion"
            "ring_buffer"    -> "rb"
            "video"          -> "vc"
            ns               -> ns

    mkNamingError (Ident name _ nameNi) =
        mkErrorInfo LevelWarn name nameNi

    globalNamesCED names (CDeclExt cde) =
        globalNamesCDE names cde
    globalNamesCED names (CFDefExt cfde) =
        globalNamesCFDE names cfde
    globalNamesCED names _ = names

    globalNamesCFDE names (CFunDef declspec dcl _ _ _) =
        if isGlobal declspec
            then globalNamesCDR names dcl
            else names

    globalNamesCDE names (CDecl declspec dcls _) =
        if isGlobal declspec
            then foldl globalNamesCDL names dcls
            else names
    globalNamesCDE names _ = names

    globalNamesCDL names (Just cdr, _, _) =
        globalNamesCDR names cdr
    globalNamesCDL names _ = names

    globalNamesCDR names (CDeclr (Just name) _ _ _ nameNi) =
        if getSrcFile nameNi == srcFile
        && (violatesNamingScheme . toLower . identToString $ name)
            then name : names
            else names
    globalNamesCDR names _ = names

    isGlobal = all $ \case
        CStorageSpec (CStatic _) -> False
        _                        -> True

    violatesNamingScheme name =
        not (
            name == namespace ||
            List.isPrefixOf (namespace ++ "_") name
        )
