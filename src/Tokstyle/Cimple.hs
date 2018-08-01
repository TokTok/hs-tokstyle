module Tokstyle.Cimple (main) where

--import           Text.Groom             (groom)
import           Tokstyle.Cimple.Lexer  (alexScanTokens, runAlex)
import           Tokstyle.Cimple.Parser (parseCimple)


main :: [String] -> IO ()
main sources = do
    putStrLn "[=] reading..."
    contents <- mapM readFile sources
    {-
    putStrLn "[=] lexing..."
    let tokens = map alexScanTokens contents
    mapM_ (putStrLn . groom) tokens
    -}
    putStrLn "[=] parsing..."
    let ast = map (`runAlex` parseCimple) contents
    -- mapM_ (putStrLn . groom) ast
    mapM_ print ast
    return ()
