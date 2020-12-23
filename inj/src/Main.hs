module Main where

import           CodeGenerator
import           Compiler
import           ErrorFormatter
import           Parser
import           Prettifier
import           TypeChecker


compileFile :: String -> IO ()
compileFile fname = do
  source <- readFile fname
  case parse fname source of
    Right ast -> do
      putStrLn (prettify ast)
      case checkTypes ast of
        (ast, []    ) -> (compile . generateCode) ast
        (_  , errors) -> printTypeErrors source errors
    Left error -> printParseError source error


main :: IO ()
main = compileFile "test.inj"
