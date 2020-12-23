{-# LANGUAGE OverloadedStrings #-}

module Compiler
  ( compile
  )
where

import           LLVM.AST
import           LLVM.Context
import           LLVM.Module             hiding ( Module )

import           Data.ByteString.Char8         as BS


root :: Global -> Module
root main = rootModule
 where
  rootModule =
    defaultModule { moduleName = "basic", moduleDefinitions = [mainDefinition] }
  mainDefinition = GlobalDefinition main


compile :: Global -> IO ()
compile main = withContext $ \ctx -> do
  llvm <- withModuleFromAST ctx (root main) moduleLLVMAssembly
  BS.putStrLn llvm
