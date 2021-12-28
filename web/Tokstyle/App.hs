{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Tokstyle.App (app) where

import           Data.ByteString          (ByteString)
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import qualified Data.Text.Encoding.Error as Text
import           Servant

import           Language.Cimple          (Lexeme, Node)
import qualified Language.Cimple.IO       as Cimple
import           Tokstyle.Linter          (analyse)


type ParseResult = Either String [Node (Lexeme Text)]

-- API specification
type TokstyleApi =
       -- Link to the source code repository, to comply with AGPL.
       "source" :> Get '[PlainText] String
       -- Parse a C file as Cimple AST.
  :<|> "parse" :> ReqBody '[OctetStream] ByteString :> Post '[JSON] ParseResult
       -- Run all Cimple analyses and return the diagnostics as list of strings.
  :<|> "analyse" :> ReqBody '[JSON] (FilePath, ParseResult) :> Post '[JSON] [Text]

tokstyleApi :: Proxy TokstyleApi
tokstyleApi = Proxy

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'Handler' monad.
server :: Server TokstyleApi
server =
         sourceH
    :<|> parseH
    :<|> analyseH
  where
    sourceH = return "https://github.com/TokTok/hs-tokstyle\n"

    parseH = return . Cimple.parseText . Text.decodeUtf8With Text.lenientDecode

    analyseH (file, Left  err) = return [Text.pack $ file <> ":" <> err]
    analyseH (file, Right ast) = return $ analyse (file, ast)

-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
app :: Application
app = serve tokstyleApi server
