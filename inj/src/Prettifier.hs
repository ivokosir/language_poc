module Prettifier
  ( prettify
  )
where

import           Data.Char
import           Data.List
import qualified LLVM.AST                      as LLVM
import qualified LLVM.AST.Constant             as C

import           AST
import           Parser


prettifyOperation :: Operation -> String
prettifyOperation Or           = "or"
prettifyOperation And          = "and"
prettifyOperation Equal        = "=="
prettifyOperation NotEqual     = "!="
prettifyOperation Greater      = ">"
prettifyOperation GreaterEqual = ">="
prettifyOperation Less         = "<"
prettifyOperation LessEqual    = "<="
prettifyOperation Add          = "+"
prettifyOperation Subtract     = "-"
prettifyOperation Multiply     = "*"
prettifyOperation Divide       = "/"


prettifyConst :: C.Constant -> String
prettifyConst (C.Null _  ) = "()"
prettifyConst (C.Int 1  1) = "true"
prettifyConst (C.Int 1  0) = "false"
prettifyConst (C.Int 32 i) = show i
prettifyConst (C.Array (LLVM.IntegerType 8) is) =
  let prettifyString [] s = "\"" ++ s ++ "\""
      prettifyString ((C.Int 8 i) : is) s =
          prettifyString is (s ++ [chr (fromInteger i)])
      prettifyString _ _ = "!unsupported_constant"
  in  prettifyString is ""
prettifyConst _ = "!unsupported_constant"


prettify_ :: String -> Expression -> String
prettify_ indent (Expression _ _ expression) =
  let nextIndent  = indent ++ "  "
      prettifySub = prettify_ nextIndent
  in  case expression of
        (Block [] e) -> "(" ++ prettifySub e ++ ")"

        (Block es e) ->
          let seperator = ";\n" ++ nextIndent
          in  "(\n"
                ++ nextIndent
                ++ intercalate seperator (fmap prettifySub es)
                ++ seperator
                ++ prettifySub e
                ++ "\n"
                ++ indent
                ++ ")"

        (Definition name e) -> name ++ " = " ++ prettify_ indent e

        (IfThenElse c t e) ->
          "if\n"
            ++ nextIndent
            ++ prettifySub c
            ++ "\n"
            ++ indent
            ++ "then\n"
            ++ nextIndent
            ++ prettifySub t
            ++ "\n"
            ++ indent
            ++ "else\n"
            ++ nextIndent
            ++ prettifySub e
            ++ "\n"
            ++ indent
            ++ "end"

        (Operation op lhs rhs) ->
          prettifySub lhs
            ++ " "
            ++ prettifyOperation op
            ++ " "
            ++ prettifySub rhs

        (Call caller callee) -> prettifySub caller ++ " " ++ prettifySub callee

        (Identifier name   ) -> name

        (Literal    const  ) -> prettifyConst const


prettify :: Expression -> String
prettify = prettify_ ""
