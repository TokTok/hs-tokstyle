{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Main (main) where

import           Control.Monad                 (unless)
import qualified Control.Monad.Parallel        as Par
import           Data.Either                   (lefts, rights)
import           Data.List                     (isPrefixOf, isSuffixOf)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Language.C                    (CTranslUnit, parseCFile,
                                                parseCFilePre)
import           Language.C.System.GCC         (newGCC)
import           Options.Applicative
import           Prettyprinter                 (Doc, defaultLayoutOptions,
                                                layoutSmart, pretty)
import           Prettyprinter.Render.Terminal (AnsiStyle, renderIO)
import           System.Exit                   (ExitCode (..), exitWith)
import           System.IO                     (hSetEncoding, stderr, stdout,
                                                utf8)
import qualified Tokstyle.C.Linter             as Linter
import           Tokstyle.C.Linter             (allWarnings, analyseProgram)


data Options = Options
    { optCC         :: String
    , optInclude    :: String
    , optMarkdown   :: Bool
    , optWarnings   :: [Text]
    , optCppOptions :: [String]
    , optFiles      :: [FilePath]
    }


parseOptions :: [Text] -> Parser Options
parseOptions allW = Options
    <$> strOption (long "cc" <> metavar "CC" <> value "clang" <> help "C compiler to use")
    <*> strOption (long "include" <> metavar "DIR" <> value "/src/workspace/hs-tokstyle/include" <> help "System include directory")
    <*> switch (long "markdown" <> help "Output documentation in Markdown format")
    <*> (processFlags . map Text.pack <$> many (strOption (short 'W' <> metavar "WARNING" <> help "Enable/disable warnings")))
    <*> many ( (("-D" ++) <$> strOption (short 'D' <> metavar "MACRO" <> help "CPP macro definition"))
           <|> (("-I" ++) <$> strOption (short 'I' <> metavar "DIR" <> help "CPP include directory"))
           <|> (("-U" ++) <$> strOption (short 'U' <> metavar "MACRO" <> help "CPP undefine macro")) )
    <*> many (argument str (metavar "FILES..."))
  where
    processFlags flags =
        let stripped = map Text.unpack flags
            initial = if any (not . ("no-" `isPrefixOf`)) stripped then [] else allW
        in foldl (flip processFlag) initial stripped

    processFlag ('n':'o':'-':f) = filter (/= Text.pack f)
    processFlag f               = (Text.pack f :)


defaultCppOpts :: String -> [String]
defaultCppOpts sysInclude =
    [ "-DCMP_NO_FLOAT"      -- avoid float->char* casts
    , "-DWORDS_BIGENDIAN=0" -- avoid casting in is_bigendian()
    , "-nostdinc"           -- we have our own stdlib headers
    , "-undef"              -- no __linux__
    , "-I" <> sysInclude
    , "-I" <> sysInclude <> "/opus"
    , "-Wno-unused-command-line-argument"
    ]


parseFile :: String -> String -> [String] -> FilePath -> IO (Either (Doc AnsiStyle) (FilePath, CTranslUnit))
parseFile cc sysInclude cppOpts file = do
    result <- if ".i" `isSuffixOf` file
              then parseCFilePre file
              else parseCFile (newGCC cc) Nothing (defaultCppOpts sysInclude ++ cppOpts) file
    return $ case result of
        Left err -> Left (pretty file <> ": Parse Error: " <> pretty (show err))
        Right tu -> Right (file, tu)


main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    opts <- execParser $ info (parseOptions allWarnings <**> helper) fullDesc
    if optMarkdown opts
        then Text.putStr Linter.markdown
        else do
            parsed <- Par.mapM (parseFile (optCC opts) (optInclude opts) (optCppOptions opts)) (optFiles opts)
            let parseErrors = lefts parsed
            errs <- concat <$> mapM Linter.renderIO (analyseProgram (optWarnings opts) (rights parsed))
            mapM_ render (parseErrors ++ errs)
            unless (null parseErrors && null errs) $ exitWith (ExitFailure 1)
  where
    render doc = renderIO stderr (layoutSmart defaultLayoutOptions doc) >> Text.hPutStrLn stderr ""
