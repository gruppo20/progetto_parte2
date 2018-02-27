module SkelClike where

-- Haskell module generated by the BNF converter

import AbsClike
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident string -> failure x
transTFloat :: TFloat -> Result
transTFloat x = case x of
  TFloat string -> failure x
transWriteIntPP :: WriteIntPP -> Result
transWriteIntPP x = case x of
  WriteIntPP string -> failure x
transWriteFloatPP :: WriteFloatPP -> Result
transWriteFloatPP x = case x of
  WriteFloatPP string -> failure x
transWriteCharPP :: WriteCharPP -> Result
transWriteCharPP x = case x of
  WriteCharPP string -> failure x
transWriteStringPP :: WriteStringPP -> Result
transWriteStringPP x = case x of
  WriteStringPP string -> failure x
transReadIntPP :: ReadIntPP -> Result
transReadIntPP x = case x of
  ReadIntPP string -> failure x
transReadFloatPP :: ReadFloatPP -> Result
transReadFloatPP x = case x of
  ReadFloatPP string -> failure x
transReadCharPP :: ReadCharPP -> Result
transReadCharPP x = case x of
  ReadCharPP string -> failure x
transReadStringPP :: ReadStringPP -> Result
transReadStringPP x = case x of
  ReadStringPP string -> failure x
transBoolean :: Boolean -> Result
transBoolean x = case x of
  Boolean_true -> failure x
  Boolean_false -> failure x
transRExpr :: RExpr -> Result
transRExpr x = case x of
  OrOp rexpr1 rexpr2 _ -> failure x
  AndOp rexpr1 rexpr2 _ -> failure x
  NotOp rexpr _ -> failure x
  EqOp rexpr1 rexpr2 _ -> failure x
  NeqOp rexpr1 rexpr2 _ -> failure x
  LtOp rexpr1 rexpr2 _ -> failure x
  LtEOp rexpr1 rexpr2 _ -> failure x
  GtOp rexpr1 rexpr2 _ -> failure x
  GtEOp rexpr1 rexpr2 _ -> failure x
  AddOp rexpr1 rexpr2 _ -> failure x
  SubOp rexpr1 rexpr2 _ -> failure x
  MulOp rexpr1 rexpr2 _ -> failure x
  DivOp rexpr1 rexpr2 _ -> failure x
  ModOp rexpr1 rexpr2 _ -> failure x
  NegOp rexpr _ -> failure x
  RefOp lexpr _ -> failure x
  FCall funcall -> failure x
  Int integer -> failure x
  Char char -> failure x
  String string -> failure x
  Real tfloat -> failure x
  Bool boolean -> failure x
  Lexpr lexpr -> failure x
transFunCall :: FunCall -> Result
transFunCall x = case x of
  Call ident rexprs _ -> failure x
  WIntCall writeintpp rexpr -> failure x
  WFloatCall writefloatpp rexpr -> failure x
  WCharCall writecharpp rexpr -> failure x
  WStringCall writestringpp rexpr -> failure x
  RIntCall readintpp -> failure x
  RFloatCall readfloatpp -> failure x
  RCharCall readcharpp -> failure x
  RStringCall readstringpp -> failure x
transLExpr :: LExpr -> Result
transLExpr x = case x of
  PreInc lexpr _ -> failure x
  PreDecr lexpr _ -> failure x
  PostInc lexpr _ -> failure x
  PostDecr lexpr _ -> failure x
  BasLExpr blexpr -> failure x
transBLExpr :: BLExpr -> Result
transBLExpr x = case x of
  ArrayEl blexpr rexpr _ -> failure x
  Id ident -> failure x
  Deref blexpr _ -> failure x
transProgram :: Program -> Result
transProgram x = case x of
  Progr constancts declars -> failure x
transDeclar :: Declar -> Result
transDeclar x = case x of
  VarDecl type_ morevars _ -> failure x
  FunDecl ident parameters _ -> failure x
  ProcDecl ident parameters blockstmt _ -> failure x
transMoreVar :: MoreVar -> Result
transMoreVar x = case x of
  VarInit ident init -> failure x
transInit :: Init -> Result
transInit x = case x of
  Init1 -> failure x
  Init2 complexrexpr -> failure x
transConstanct :: Constanct -> Result
transConstanct x = case x of
  Const ident type_ rexpr _ -> failure x
transComplexRExpr :: ComplexRExpr -> Result
transComplexRExpr x = case x of
  Simple rexpr -> failure x
  Array complexrexprs -> failure x
transType :: Type -> Result
transType x = case x of
  BasType basictype -> failure x
  CompType compositetype -> failure x
transBasicType :: BasicType -> Result
transBasicType x = case x of
  BasicType_bool -> failure x
  BasicType_char -> failure x
  BasicType_float -> failure x
  BasicType_int -> failure x
  BasicType_string -> failure x
  BasicType_void -> failure x
transCompositeType :: CompositeType -> Result
transCompositeType x = case x of
  ArrDef type_ rexpr -> failure x
  ArrNoDef type_ -> failure x
  Pointer type_ -> failure x
transParameter :: Parameter -> Result
transParameter x = case x of
  Param type_ ident -> failure x
transBlockStmt :: BlockStmt -> Result
transBlockStmt x = case x of
  ProcBlock declars stmts -> failure x
transStmt :: Stmt -> Result
transStmt x = case x of
  MoreDecl declar -> failure x
  ProcCall funcall -> failure x
  IndIter whilestmt -> failure x
  Cond conditionstmt -> failure x
  Assgn blexpr assignmentop rexpr _ -> failure x
  LExprStmt lexpr -> failure x
transAssignment_op :: Assignment_op -> Result
transAssignment_op x = case x of
  Assign -> failure x
  AssgnMul -> failure x
  AssgnAdd -> failure x
  AssgnDiv -> failure x
  AssgnSub -> failure x
transConditionStmt :: ConditionStmt -> Result
transConditionStmt x = case x of
  IfNoElse rexpr blockstmt _ -> failure x
  IfYesElse rexpr blockstmt1 blockstmt2 _ -> failure x
transWhileStmt :: WhileStmt -> Result
transWhileStmt x = case x of
  While rexpr blockstmt _ -> failure x
