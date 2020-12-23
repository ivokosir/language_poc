module TypeChecker
  ( Error(..)
  , checkTypes
  )
where

import           Data.Map.Strict                ( Map
                                                , (!?)
                                                , insert
                                                , empty
                                                )
import           Data.Maybe
import           LLVM.AST                       ( Type )
import           LLVM.AST.Typed                 ( typeOf )
import           Text.Parsec                    ( SourcePos )

import           AST
import           Parser


type Scope = Map String Type


data Error = Error SourcePos SourcePos String deriving (Show)


addToScope :: Expression -> Type -> Scope -> Scope
addToScope (Expression _ _ (Definition name _)) type_ scope =
  insert name type_ scope
addToScope _ _ scope = scope


createError :: Expression -> String -> Error
createError (Expression start end _) = Error start end


getRequested :: Expression -> Type -> Maybe Type -> [Error]
getRequested _ _ Nothing = []
getRequested expression actual (Just requested) =
  let errorString =
          "actual type '"
            ++ show actual
            ++ "' does not match requested type '"
            ++ show requested
            ++ "'"
      error = createError expression errorString
  in  ([ error | actual /= requested ])


setExpression :: BaseExpression -> Expression -> Expression
setExpression e (Expression start end _) = Expression start end e


checkExpression
  :: Maybe Type -> Scope -> Expression -> (Expression, Type, [Error])
checkExpression requestedType scope0 expression@(Expression _ _ (Block es last))
  = let checkSub scope [] tes errors = (tes, scope, errors)
        checkSub scopePrev (e : es) tes errorsPrev =
            let (te, type_, errors) = checkExpression Nothing scopePrev e
                scopeNext           = addToScope te type_ scopePrev
            in  checkSub scopeNext es (tes ++ [te]) (errorsPrev ++ errors)
        (tes, scope1, errors    ) = checkSub scope0 es [] []
        (te , type_ , lastErrors) = checkExpression requestedType scope1 last
    in  (setExpression (Block tes te) expression, type_, errors ++ lastErrors)

checkExpression requestedType scope expression@(Expression _ _ (Definition name e))
  = let (e_, type_, errors) = checkExpression requestedType scope e
    in  (setExpression (Definition name e_) expression, type_, errors)

checkExpression requestedType scope expression@(Expression _ _ (IfThenElse condition thenE elseE))
  = let (condition_, _, conditionErrors) =
            checkExpression (Just tBool) scope condition
        (thenE_, thenType, thenErrors) =
            checkExpression requestedType scope thenE
        (elseE_, _, elseErrors) = checkExpression (Just thenType) scope elseE
        allErrors               = conditionErrors ++ thenErrors ++ elseErrors
    in  ( setExpression (IfThenElse condition_ thenE_ elseE_) expression
        , thenType
        , allErrors
        )

checkExpression requestedType scope expression@(Expression _ _ (Operation op lhs rhs))
  = let (lhsType, rhsType, type_    ) = operatonTypes op
        (lhs_   , _      , lhsErrors) = checkExpression (Just lhsType) scope lhs
        (rhs_   , _      , rhsErrors) = checkExpression (Just rhsType) scope rhs
        errors = getRequested expression type_ requestedType
    in  ( setExpression (Operation op lhs_ rhs_) expression
        , type_
        , lhsErrors ++ rhsErrors ++ errors
        )

checkExpression requestedType scope expression@(Expression _ _ (Call caller callee))
  = let
      (callee_, calleeType, calleeErrors) =
        checkExpression Nothing scope callee
      retType                    = fromMaybe tUnit requestedType
      (caller_, _, callerErrors) = checkExpression
        (Just (createFunctionType retType calleeType))
        scope
        caller
    in
      ( setExpression (Call caller_ callee_) expression
      , retType
      , callerErrors ++ calleeErrors
      )

checkExpression requestedType scope expression@(Expression _ _ (Identifier name))
  = let (type_, errors) = case scope !? name of
          Nothing ->
            let errorString =
                    "name '" ++ name ++ "' does not exist in this scope"
                error = createError expression errorString
            in  (fromMaybe tUnit requestedType, [error])
          Just actual -> (actual, getRequested expression actual requestedType)
    in  (setExpression (Identifier name) expression, type_, errors)

checkExpression requestedType _ expression@(Expression _ _ (Literal literal)) =
  let type_  = typeOf literal
      errors = getRequested expression type_ requestedType
  in  (setExpression (Literal literal) expression, type_, errors)


checkTypes :: Expression -> (Expression, [Error])
checkTypes e =
  let (e_, _, errors) = checkExpression Nothing empty e in (e_, errors)
