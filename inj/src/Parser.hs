module Parser
  ( Expression(..)
  , BaseExpression(..)
  , parse
  )
where

import           Text.Parsec             hiding ( parse )
import           Text.Parsec.Expr
import           Text.Parsec.Language
import           Text.Parsec.String
import qualified LLVM.AST.Constant             as C
import qualified Text.Parsec                   as P
                                                ( parse )
import qualified Text.Parsec.Token             as P

import           AST


data Expression = Expression
  { expressionStart :: SourcePos
  , expressionEnd :: SourcePos
  , expressionBase :: BaseExpression }
  deriving (Show)


data BaseExpression
  = Block [Expression] Expression
  | Definition String Expression
  | IfThenElse Expression Expression Expression
  | Operation Operation Expression Expression
  | Call Expression Expression
  | Identifier String
  | Literal C.Constant
  deriving (Show)


languageDef = emptyDef
  { P.commentStart    = "/*"
  , P.commentEnd      = "*/"
  , P.commentLine     = "//"
  , P.reservedOpNames = [ "*"
                        , "/"
                        , "+"
                        , "-"
                        , "*"
                        , "/"
                        , ">"
                        , ">="
                        , "<"
                        , "<="
                        , "=="
                        , "!="
                        , "and"
                        , "or"
                        ]
  , P.reservedNames   = [ "and"
                        , "or"
                        , "true"
                        , "false"
                        , "if"
                        , "then"
                        , "else"
                        , "end"
                        ]
  }


lexer = P.makeTokenParser languageDef


identifier = P.identifier lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer
parens = P.parens lexer
natural = P.natural lexer
semiSep = P.semiSep lexer
whiteSpace = P.whiteSpace lexer
stringLiteral = P.stringLiteral lexer


withPosition :: Parser BaseExpression -> Parser Expression
withPosition p = do
  start <- getPosition
  ast   <- p
  end   <- getPosition
  return $ Expression start end ast


formatBlockBody :: [Expression] -> BaseExpression
formatBlockBody [] = Literal unitConst
formatBlockBody es = Block (init es) (last es)


parser :: Parser Expression
parser = do
  whiteSpace
  ast <- withPosition (formatBlockBody <$> blockBody)
  eof
  return ast


block :: Parser Expression
block = withPosition (formatBlockBody <$> parens blockBody)


blockBody :: Parser [Expression]
blockBody = semiSep expression


expression :: Parser Expression
expression = try definition <|> ifThenElse <|> operators


definition :: Parser Expression
definition = withPosition $ do
  name <- identifier
  reservedOp "="
  Definition name <$> expression


ifThenElse :: Parser Expression
ifThenElse = withPosition $ do
  reserved "if"
  condition <- expression
  reserved "then"
  thenExpression <- expression
  reserved "else"
  elseExpression <- expression
  reserved "end"
  return $ IfThenElse condition thenExpression elseExpression


operators :: Parser Expression
operators = buildExpressionParser operatorsTable call


operatorsTable =
  let toOperaton operation lhs rhs = Expression (expressionStart lhs)
                                                (expressionEnd rhs)
                                                (Operation operation lhs rhs)
      binary name operation =
          Infix (reservedOp name >> return (toOperaton operation)) AssocLeft
  in  [ [binary "*" Multiply, binary "/" Divide]
      , [binary "+" Add, binary "-" Subtract]
      , [binary "*" Multiply, binary "/" Divide]
      , [ binary ">"  Greater
        , binary ">=" GreaterEqual
        , binary "<"  Less
        , binary "<=" LessEqual
        ]
      , [binary "==" Equal, binary "!=" NotEqual]
      , [binary "and" And]
      , [binary "or" Or]
      ]


call :: Parser Expression
call = do
  caller <- primary
  (do
      callee <- call
      return
        (Expression (expressionStart caller)
                    (expressionEnd callee)
                    (Call caller callee)
        )
    )
    <|> return caller


primary :: Parser Expression
primary =
  let primary_ =
          (reserved "true" >> return (Literal trueConst))
            <|> (reserved "false" >> return (Literal falseConst))
            <|> Literal
            .   int32Const
            <$> natural
            <|> Literal
            .   stringConst
            <$> stringLiteral
            <|> Identifier
            <$> identifier
  in  withPosition primary_ <|> block


parse :: SourceName -> String -> Either ParseError Expression
parse = P.parse parser
