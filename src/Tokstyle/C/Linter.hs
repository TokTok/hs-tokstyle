{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter
    ( analyse
    , analyseText
    , allWarnings
    , markdown
    , renderIO
    , renderPure
    , LinterError
    , DiagnosticLevel(..)
    , Doc
    , AnsiStyle
    , diagToText
    ) where

import qualified Data.ByteString                      as BS
import           Data.List                            (sortBy)
import           Data.Map.Strict                      (Map)
import qualified Data.Map.Strict                      as Map
import           Data.Text                            (Text)
import qualified Data.Text                            as Text
import qualified Data.Text.Encoding                   as Text
import           Language.C.Analysis.AstAnalysis      (analyseAST)
import           Language.C.Analysis.SemRep           (GlobalDecls)
import           Language.C.Analysis.TravMonad        (CLanguage (..), Trav,
                                                       TravOptions (..),
                                                       modifyOptions,
                                                       modifyUserState, runTrav,
                                                       userState)
import           Language.C.Data.Error                (CError, errorPos)
import           Language.C.Data.Position             (isSourcePos)
import qualified Language.C.Data.Position             as C
import           Language.C.Syntax.AST                (CTranslUnit)
import           Language.Cimple.Diagnostics          (Diagnostic (..),
                                                       DiagnosticLevel (..),
                                                       IsPosition (..),
                                                       diagToText)
import qualified Language.Cimple.Diagnostics          as Diagnostics

import           Prettyprinter                        (Doc, pretty)
import           Prettyprinter.Render.Terminal        (AnsiStyle)
import qualified Tokstyle.C.Env                       as Env
import           Tokstyle.C.Env                       (DiagnosticLevel (..),
                                                       Env, LinterError,
                                                       defaultEnv, linterErrors)

import           Data.Function                        (on)
import qualified Tokstyle.C.Linter.BoolConversion     as BoolConversion (descr)
import qualified Tokstyle.C.Linter.BorrowCheck        as BorrowCheck (descr)
import qualified Tokstyle.C.Linter.CallbackData       as CallbackData (descr)
import qualified Tokstyle.C.Linter.CallbackDiscipline as CallbackDiscipline (descr)
import qualified Tokstyle.C.Linter.CallbackParams     as CallbackParams (descr)
import qualified Tokstyle.C.Linter.Cast               as Cast (descr)
import qualified Tokstyle.C.Linter.Conversion         as Conversion (descr)
import qualified Tokstyle.C.Linter.Memcpy             as Memcpy (descr)
import qualified Tokstyle.C.Linter.Memset             as Memset (descr)
import qualified Tokstyle.C.Linter.Memzero            as Memzero (descr)
import qualified Tokstyle.C.Linter.SizeArg            as SizeArg (descr)
import qualified Tokstyle.C.Linter.Sizeof             as Sizeof (descr)
import qualified Tokstyle.C.Linter.StrictTypedef      as StrictTypedef (descr)
import qualified Tokstyle.C.Linter.VoidCall           as VoidCall (descr)


type Linter = (GlobalDecls -> Trav Env (), (Text, Text))

linters :: [Linter]
linters =
    [ BoolConversion.descr
    , BorrowCheck.descr
    , CallbackData.descr
    , CallbackDiscipline.descr
    , CallbackParams.descr
    , Cast.descr
    , Conversion.descr
    , Memcpy.descr
    , Memset.descr
    , Memzero.descr
    , SizeArg.descr
    , Sizeof.descr
    , StrictTypedef.descr
    , VoidCall.descr
    ]


runLinters :: [Text] -> GlobalDecls -> Trav Env ()
runLinters flags tu =
    mapM_ (\(f, (flag, _)) -> do
        modifyUserState $ \env -> env{Env.currentFlag = Just flag}
        f tu
        modifyUserState $ \env -> env{Env.currentFlag = Nothing}
    ) . filter ((`elem` flags) . fst . snd) $ linters


analyse :: [Text] -> CTranslUnit -> [LinterError]
analyse enabled tu =
    case analysis of
        Left errs        -> map toLinterError errs
        Right (_, state) -> reverse . linterErrors $ userState state
  where
    analysis = runTrav defaultEnv $ do
        modifyOptions (\opts -> opts { language = GNU99 })
        decls <- analyseAST tu
        runLinters enabled decls

    toLinterError :: CError -> LinterError
    toLinterError err = Diagnostic (Env.CPosition (errorPos err)) 1 ErrorLevel (pretty (show err)) Nothing [] []


analyseText :: [Text] -> CTranslUnit -> [Text]
analyseText enabled tu = map diagToText (analyse enabled tu)


renderIO :: [LinterError] -> IO [Doc AnsiStyle]
renderIO errors = do
    let files = Map.keys . Map.fromList $ [ (posFile (diagPos e), ()) | e <- errors, isRealPos (diagPos e) ]
    cache <- Map.fromList <$> mapM (\f -> do
        ls <- Text.lines . Text.decodeUtf8 <$> BS.readFile f
        return (f, ls)) files
    return $ renderPure cache errors


renderPure :: Map FilePath [Text] -> [LinterError] -> [Doc AnsiStyle]
renderPure = Diagnostics.renderPure


allWarnings :: [Text]
allWarnings = map (fst . snd) linters


markdown :: Text
markdown = Text.intercalate "\n" . (prelude ++) . map mkDoc . sortBy (compare `on` (fst . snd)) $ linters
  where
    prelude =
        [ "# C-based linters (`check-c`)"
        , ""
        , "There are currently " <> Text.pack (show $ length linters) <> " linters implemented."
        , ""
        ]
    mkDoc (_, (flag, doc)) = "## `-W" <> flag <> "`\n\n" <> Text.stripEnd doc <> "\n"
