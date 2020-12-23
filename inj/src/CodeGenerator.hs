module CodeGenerator
  ( generateCode
  )
where

import           Data.Map.Strict         hiding ( foldl )
import           LLVM.AST                hiding ( Call )
import           LLVM.AST.Global
import           LLVM.AST.Typed                 ( typeOf )
import qualified LLVM.AST                      as LLVM
                                                ( Instruction(Call) )
import qualified LLVM.AST.CallingConvention    as CC

import           AST
import           Parser


data State = State
  { unnamedCounter :: Word
  , nextNames :: Map String Word
  , references :: Map String Operand
  , blocks :: [BasicBlock]
  , currentName :: Name
  , instructions :: [Named Instruction] }
  deriving (Show)


initialState = State { unnamedCounter = 1
                     , nextNames      = empty
                     , references     = empty
                     , blocks         = []
                     , currentName    = UnName 0
                     , instructions   = []
                     }


toParentScope :: State -> State -> State
toParentScope parent state = state { references = references parent }


addReference :: String -> Operand -> State -> State
addReference name value state =
  state { references = insert name value (references state) }


generateName :: Maybe String -> State -> (State, Name)
generateName Nothing state0 =
  let i      = unnamedCounter state0
      state1 = state0 { unnamedCounter = i + 1 }
  in  (state1, UnName i)
generateName (Just name) state0 =
  let i     = nextNames state0 !? name
      nextI = case i of
        Nothing -> 0
        Just i  -> i + 1
      state1     = state0 { nextNames = insert name nextI (nextNames state0) }
      uniqueName = mkName (name ++ maybe "" (\id -> "." ++ show id) i)
  in  (state1, uniqueName)


addInstruction :: Maybe String -> Instruction -> State -> (State, Name)
addInstruction requestedName instruction state0 =
  let (state1, name) = generateName requestedName state0
  in  ( state1 { instructions = instructions state1 ++ [name := instruction] }
      , name
      )


endBlock :: Name -> Terminator -> State -> State
endBlock nextName terminator state = state
  { blocks       = blocks state
                     ++ [ BasicBlock (currentName state)
                                     (instructions state)
                                     (Do terminator)
                        ]
  , instructions = []
  , currentName  = nextName
  }


generate :: Maybe String -> State -> Expression -> (State, Operand)
generate requestedName state0 (Expression _ _ (Block es last)) =
  let generateSub state e = fst $ generate Nothing state e
      state1          = foldl generateSub state0 es
      (state2, value) = generate requestedName state1 last
  in  (toParentScope state0 state2, value)

generate Nothing state0 (Expression _ _ (Definition name e)) =
  let (state1, value) = generate (Just name) state0 e
      state2          = addReference name value state1
  in  (state2, value)
generate requestedName state (Expression _ _ (Definition _ e)) =
  generate requestedName state e

generate requestedName state0 (Expression _ _ (IfThenElse condition thenE elseE))
  = let
      (state1, conditionValue) = generate Nothing state0 condition
      (state2, thenName      ) = generateName Nothing state1
      (state3, elseName      ) = generateName Nothing state2
      (state4, endName       ) = generateName Nothing state3
      state5 =
        endBlock thenName (CondBr conditionValue thenName elseName []) state4
      (state6, thenValue) =
        generate Nothing (toParentScope state0 state5) thenE
      state7 = endBlock elseName (Br endName []) state6
      (state8, elseValue) =
        generate Nothing (toParentScope state0 state7) elseE
      state9          = endBlock endName (Br endName []) state8
      type_           = typeOf thenValue
      phi = Phi type_ [(thenValue, thenName), (elseValue, elseName)] []
      (state10, name) = addInstruction requestedName phi state9
    in
      (toParentScope state0 state10, LocalReference type_ name)

generate requestedName state0 (Expression _ _ (Operation op lhs rhs)) =
  let (state1, lhsValue) = generate Nothing state0 lhs
      (state2, rhsValue) = generate Nothing state1 rhs
      binary             = operationToInstruction op lhsValue rhsValue
      (_, _, type_)      = operatonTypes op
      (state3, name)     = addInstruction requestedName binary state2
  in  (toParentScope state0 state3, LocalReference type_ name)

generate requestedName state0 (Expression _ _ (Call caller callee)) =
  let
    (state1, callerValue) = generate Nothing state0 caller
    (state2, calleeValue) = generate Nothing state1 callee
    Just (retType, _)     = functionType (typeOf callerValue)
    call =
      LLVM.Call Nothing CC.C [] (Right callerValue) [(calleeValue, [])] [] []
    (state3, name) = addInstruction requestedName call state2
  in
    (toParentScope state0 state3, LocalReference retType name)

generate _ state (Expression _ _ (Identifier name)) =
  (state, references state ! name)

generate _ state (Expression _ _ (Literal literal)) =
  (state, ConstantOperand literal)


generateCode :: Expression -> Global
generateCode e =
  let (state0, value) = generate Nothing initialState e
      (state1, name ) = generateName Nothing state0
      state2          = endBlock name (Ret (Just value) []) state1
  in  functionDefaults { name        = mkName "main"
                       , parameters  = ([], False)
                       , returnType  = typeOf value
                       , basicBlocks = blocks state2
                       }
