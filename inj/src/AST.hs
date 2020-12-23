module AST
  ( Operation(..)
  , tUnit
  , tBool
  , tInt32
  , functionType
  , createFunctionType
  , unitConst
  , trueConst
  , falseConst
  , int32Const
  , stringConst
  , operatonTypes
  , operationToInstruction
  )
where

import           Data.Char                      ( ord )
import qualified LLVM.AST                      as LLVM
import qualified LLVM.AST.Constant             as C
import qualified LLVM.AST.IntegerPredicate     as IPred


data Operation
  = Or
  | And
  | Equal
  | NotEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | Add
  | Subtract
  | Multiply
  | Divide
  deriving (Show)


data AConst
  = CUinit
  | CBool Bool
  | CInt Int
  | CString String
  deriving (Show)


data AType
  = TUnit
  | TBool
  | TInt
  | TString
  | TFunction AType AType
  deriving (Show)


constType :: AConst -> AType
constType CUinit      = TUnit
constType (CBool   _) = TBool
constType (CInt    _) = TInt
constType (CString _) = TString


data Expression
  = Block [Expression] Expression
  | Definition String Expression
  | IfThenElse Int AType Expression Expression Expression Int Int
  | Operation Int Operation Expression Expression
  | Call Int AType Expression Expression
  | Function Int AType String [(AType, String)] Expression
  | Identifier AType String
  | Literal AConst
  deriving (Show)


tUnit = LLVM.VoidType
tBool = LLVM.IntegerType 1
tInt32 = LLVM.IntegerType 32


functionType :: LLVM.Type -> Maybe (LLVM.Type, LLVM.Type)
functionType (LLVM.FunctionType caller [callee] False) = Just (caller, callee)
functionType _ = Nothing


createFunctionType :: LLVM.Type -> LLVM.Type -> LLVM.Type
createFunctionType caller callee = LLVM.FunctionType caller [callee] False


unitConst :: C.Constant
unitConst = C.Null tUnit

trueConst :: C.Constant
trueConst = C.Int 1 1

falseConst :: C.Constant
falseConst = C.Int 1 0

int32Const :: Integer -> C.Constant
int32Const = C.Int 32

stringConst :: String -> C.Constant
stringConst string =
  C.Array (LLVM.IntegerType 8) (fmap (C.Int 8 . toInteger . ord) string)


operatonTypes :: Operation -> (LLVM.Type, LLVM.Type, LLVM.Type)
operatonTypes Or           = (tBool, tBool, tBool)
operatonTypes And          = (tBool, tBool, tBool)
operatonTypes Equal        = (tInt32, tInt32, tBool)
operatonTypes NotEqual     = (tInt32, tInt32, tBool)
operatonTypes Greater      = (tInt32, tInt32, tBool)
operatonTypes GreaterEqual = (tInt32, tInt32, tBool)
operatonTypes Less         = (tInt32, tInt32, tBool)
operatonTypes LessEqual    = (tInt32, tInt32, tBool)
operatonTypes Add          = (tInt32, tInt32, tInt32)
operatonTypes Subtract     = (tInt32, tInt32, tInt32)
operatonTypes Multiply     = (tInt32, tInt32, tInt32)
operatonTypes Divide       = (tInt32, tInt32, tInt32)


operationToInstruction
  :: Operation -> LLVM.Operand -> LLVM.Operand -> LLVM.Instruction
operationToInstruction Or           lhs rhs = LLVM.Or lhs rhs []
operationToInstruction And          lhs rhs = LLVM.And lhs rhs []
operationToInstruction Equal        lhs rhs = LLVM.ICmp IPred.EQ lhs rhs []
operationToInstruction NotEqual     lhs rhs = LLVM.ICmp IPred.NE lhs rhs []
operationToInstruction Greater      lhs rhs = LLVM.ICmp IPred.SGT lhs rhs []
operationToInstruction GreaterEqual lhs rhs = LLVM.ICmp IPred.SGE lhs rhs []
operationToInstruction Less         lhs rhs = LLVM.ICmp IPred.SLT lhs rhs []
operationToInstruction LessEqual    lhs rhs = LLVM.ICmp IPred.SLE lhs rhs []
operationToInstruction Add          lhs rhs = LLVM.Add False False lhs rhs []
operationToInstruction Subtract     lhs rhs = LLVM.Sub False False lhs rhs []
operationToInstruction Multiply     lhs rhs = LLVM.Mul False False lhs rhs []
operationToInstruction Divide       lhs rhs = LLVM.SDiv False lhs rhs []
