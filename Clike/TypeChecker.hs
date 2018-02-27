module TypeChecker where

import AbsClike
import Control.Monad
import qualified Data.Map as M
import ErrM
import PrintClike

-- Ogni ambiente è definito da
-- costanti, segnature di funzioni, variabili 
-- e segnature di procedure
type Env = [(Const, Sig, Context, Pro)]

-- Le costanti sono caratterizzate da
-- nome, tipo
type Const = M.Map Ident Type

-- Le segnature sono caratterizzate da
-- nome della funzione e tipi dei parametri
type Sig = M.Map Ident ([Type])
type Pro = M.Map Ident ([Type])

-- Le variabili dichiarate richiedono
-- nome e tipo
type Context = M.Map Ident Type

-- Funzioni predefinite
defFun = M.insert (Ident "writeInt") ([BasType BasicType_int]) $
  M.insert (Ident "writeFloat") ([BasType BasicType_float]) $
  M.insert (Ident "writeChar") ([BasType BasicType_char]) $
  M.insert (Ident "writeString") ([BasType BasicType_string]) $
  M.insert (Ident "readInt") ([]) $
  M.insert (Ident "readFloat") ([]) $
  M.insert (Ident "readChar") ([]) $
  M.insert (Ident "readString") ([]) M.empty
  
-- Typecheck
typecheck :: Program -> Err Env
typecheck (Progr const decl) = do
  -- estensione dell'ambiente con le dichiarazioni di costanti
  env' <- foldM extendConst [(M.empty, defFun, M.empty, M.empty)] const

  -- estensione dell'ambiente con le dichiarazioni di funzioni
  env'' <- foldM extendDecl env' decl
  
  -- controllo delle costanti
  foldM checkCon env'' const
  
  -- controllo delle dichiarazioni
  foldM checkDef env'' decl
  
-- Entrata in un nuovo blocco
newBlock  :: Env -> Err Env
newBlock env = return $ (M.empty,M.empty,M.empty,M.empty):env

-- Uscita da un blocco
exitBlock :: Env -> Err Env
exitBlock (x:xs) = return xs

-- Estensione dell'ambiente con le dichiarazioni di costanti
extendConst :: Env -> Constanct -> Err Env
extendConst env@((con,sig,ctx,pro):xs) const =
  case const of
    (Const id typ _ (line, _)) -> 
      case M.lookup id con of
        Nothing -> return ((addConst con id typ, sig, ctx, pro):xs)
        Just _ -> throwError line False $ "Constant (" ++ printTree id ++ ") was already declared"

-- Aggiunta di una costante all'ambiente    
addConst :: Const -> Ident -> Type -> Const
addConst const id typ = M.insert id typ const

-- Estensione dell'ambiente con le dichiarazioni di funzione
extendDecl :: Env -> Declar -> Err Env
extendDecl env@((con,sig,ctx,pro):xs) decl = 
  case decl of
    (FunDecl id par (line,_)) ->
      case M.lookup id $ M.union sig defFun of
        Nothing -> return ((con, addFun sig id par, ctx, pro):xs)
        Just _ -> throwError line True $ "Function (" ++ printTree id ++ ") was already declared"
    (ProcDecl id par _ (line,_)) ->
      case M.lookup id $ M.union pro defFun of
        Nothing -> lookFunDef env line id env
        Just _ -> throwError line True $ "Procedure (" ++ printTree id ++ ") was already declared"
    _ -> Ok env
    
-- Ricerca di una funzione nell'ambiente locale e non locale
lookFunDef :: Env -> Int -> Ident -> Env -> Err Env
lookFunDef ((_,sig,_,_):xs) line id e = case M.lookup id sig of
  Nothing -> lookFunDef xs line id e
  Just typ -> Ok e
lookFunDef [] line id e = throwError line False $ "Function (" ++ printTree id ++ ") was not previously declared"
    
-- Aggiunta della dichiarazione di una funzione all'ambiente
addFun :: Sig -> Ident -> [Parameter] -> Sig
addFun sig id p = M.insert id (foldl addParam [] p) sig
  where
    addParam l (Param typ _) = l ++ [typ]
    
-- Aggiunta della dichiarazione di una procedura all'ambiente
addPro :: Pro -> Ident -> [Parameter] -> Sig
addPro pro id p = M.insert id (foldl addParam [] p) pro
  where
    addParam l (Param typ _) = l ++ [typ]
    
-- Aggiunta variabile all'ambiente
addVar :: Type -> Int -> Env -> Ident -> Err Env
addVar typ line ((con,sig,ctx,pro):xs) id =
  case M.lookup id ctx of
    Nothing -> Ok ((con, sig, M.insert id typ ctx, pro):xs)
    Just _ -> throwError line True $ "Variable (" ++ printTree id ++ ") was already declared"
addVar typ line [] id  = Ok []
    
-- Risoluzione dichiarazione
checkDef :: Env -> Declar -> Err Env
checkDef env st =
  case st of
    FunDecl id par (line,_) -> return env
    ProcDecl id par stmt (line,_) -> checkProc env id par stmt
    VarDecl typ more (line,_) -> do foldM (addMoreVar typ line) env more

addMoreVar :: Type -> Int -> Env -> MoreVar -> Err Env
addMoreVar typ line env more = 
  case more of
    VarInit id _ -> addVar typ line env id
    
-- Risoluzione di dichiarazioni di costante
checkCon :: Env -> Constanct -> Err Env
checkCon env st =
  case st of
    Const id typ exp (line,_) -> do
      checkExp env line typ (Simple exp)
      return env
      
-- Controllo della definizione di una funzione
checkProc :: Env -> Ident -> [Parameter] -> BlockStmt -> Err Env
checkProc env id par (ProcBlock decl stmt) = do 

  -- entrata nel nuovo blocco
  env' <- newBlock env

  -- aggiunta dei parametri della funzione all'ambiente
  env'' <- foldM addFunPar env' par

  -- estensione dell'ambiente con le dichiarazioni di funzioni e label
  env''' <- foldM extendDecl env'' decl

  -- controllo le dichiarazioni
  env_ <- foldM checkDef env''' decl

  -- controllo gli statement
  envf <- foldM (checkStmt False) env_ stmt

  -- uscita dal blocco
  exitBlock envf
  
-- aggiunta dei parametri della funzione all'ambiente
addFunPar env par = case par of
  Param typ id -> addVar typ 0 env id
  
-- Controllo della corrispondenza tra tipo espressione e tipo atteso
checkExp :: Env -> Int -> Type -> ComplexRExpr -> Err ()
checkExp env line typ exp = case exp of
  (Simple e) -> do
    typ2 <- inferExp env e
    if (elem typ [BasType BasicType_int,BasType BasicType_float])
      then if (elem typ2 [BasType BasicType_int,BasType BasicType_float])
        then return ()
        else throwError line True $  "Type of (" ++ printTree e ++
             ") expected numeric but found (" ++ printTree typ2 ++ ")"
      else if (typ2 == typ) 
        then return ()
        else throwError line True $ "Type of (" ++ printTree e ++
                                 ") expected (" ++ printTree typ ++
                                 ") but found (" ++ printTree typ2 ++ ")"
  _ -> do
    typ2 <- inferComplExp env line exp
    if (typ2 == typ) then
      return ()
    else
      throwError line True $ "Type of (" ++ printTree exp ++
                          ") expected (" ++ printTree typ ++
                          ") but found (" ++ printTree typ2 ++ ")"
  
-- Controllo all'interno di cicli o condizioni
checkInside :: Env -> BlockStmt -> Err Env
checkInside env (ProcBlock decl stmt) = do 

  -- entrata nel nuovo blocco
  env' <- newBlock env

  -- estensione dell'ambiente con le dichiarazioni di funzioni e label
  env'' <- foldM extendDecl env' decl

  -- controllo le dichiarazioni
  env_ <- foldM checkDef env'' decl

  -- controllo gli statement
  envf <- foldM (checkStmt False) env_ stmt

  -- uscita dal blocco
  exitBlock envf
  
-- Controllo degli statement
checkStmt:: Bool -> Env -> Stmt -> Err Env
checkStmt b env st = case st of
  -- Altre Dichiarazioni 
  MoreDecl decl -> do checkDef env decl
  
  -- Chiamata funzione
  ProcCall (Call id exp (line, _)) -> do
    (typ) <- lookFun env line id
    checkParam id line exp typ env
    Ok env

  -- LExpr
  LExprStmt exp -> case exp of
    PreInc exp (line,_) -> do inferNumeric env line (Lexpr exp) 
                              Ok env
    PostInc exp (line,_) -> do inferNumeric env line (Lexpr exp) 
                               Ok env
    PreDecr exp (line,_) -> do inferNumeric env line (Lexpr exp) 
                               Ok env
    PostDecr exp (line,_) -> do inferNumeric env line (Lexpr exp) 
                                Ok env 
    BasLExpr exp -> do inferBLExpr env 0 exp
                       Ok env

  -- While Statement
  IndIter it -> case it of
    (While exp stmt (line,_)) -> do
      checkExp env line (BasType BasicType_bool) (Simple exp)
      checkInside env stmt
 
  -- Condition Statement
  Cond sel -> case sel of
    (IfNoElse exp stmt (line,_) ) -> do
      checkExp env line (BasType BasicType_bool) (Simple exp)
      checkInside env stmt
    (IfYesElse exp stmt stmt1 (line,_)) -> do
      checkExp env line (BasType BasicType_bool) (Simple exp)
      env' <- checkInside env stmt
      checkInside env' stmt1

  -- Assegnamenti
  Assgn bexpr assop exp (line,_) -> case assop of
    Assign -> do
      (typ, b) <- inferBLExpr env line bexpr 
      if b == False
        then throwError line False $ "Variable (" ++ printTree bexpr ++ ") is a constant"
        else do
          checkExp env line typ (Simple exp)
          Ok env
    AssgnMul -> checkAssgNumeric env line bexpr exp
    AssgnAdd -> checkAssgNumeric env line bexpr exp
    AssgnDiv -> checkAssgNumeric env line bexpr exp
    AssgnSub -> checkAssgNumeric env line bexpr exp
    
-- Controllo degli assegnamenti numerici ( *=, +=, /=, -= )
checkAssgNumeric env line bexpr exp = do
  (typ, b) <- inferBLExpr env line bexpr
  if b == False
    then throwError line False $ "Variable (" ++ printTree bexpr ++ ") is a constant"
    else
      if (elem typ [BasType BasicType_int, BasType BasicType_float]) 
        then do
          checkExp env line typ (Simple exp)
          Ok env
        else throwError line True $ "Type of expression (" ++ printTree exp ++ ") expected numeric but found (" 
                     ++ printTree typ ++ ")"

-- Controllo dei parametri passati ad una funzione
checkParam:: Ident -> Int -> [RExpr] -> [Type] -> Env -> Err ()
checkParam id line exp typ env =
  if length exp == length typ
    then checkParamAux exp typ env
    else throwError line True $ "Wrong number of parameter on function (" ++ printTree id ++ ")"
      where
        checkParamAux (x:xs) (y:ys) env = checkExp env line y (Simple x)
        
-- Ricerca di una variabile nell'ambiente locale e non locale
lookVar :: Env -> Int -> Ident -> Err (Type, Bool)
lookVar ((_,_,ctx,_):xs) line id = case M.lookup id ctx of
  Nothing -> lookVar xs line id
  Just typ -> Ok (typ, True)
lookVar [] line id = throwError line False $ "Variable (" ++ printTree id ++ ") not declared"

-- Ricerca una costante nell'ambiente locale e non locale
lookCon :: Env -> Int -> Ident -> Err (Type, Bool)
lookCon ((con,_,_,_):xs) line id = case M.lookup id con of
  Nothing -> lookCon xs line id
  Just typ -> Ok (typ, False)
lookCon [] line id = throwError line False $ "Constanct (" ++ printTree id ++ ") not declared"

-- Ricerca di una funzione nell'ambiente locale e non locale
lookFun :: Env -> Int -> Ident -> Err ([Type])
lookFun ((_,sig,_,_):xs) line id = case M.lookup id sig of
  Nothing -> lookFun xs line id
  Just typ -> Ok typ
lookFun [] line id = throwError line False $ "Function (" ++ printTree id ++ ") not declared"

-- Restituisco il tipo di un espressione
inferExp :: Env -> RExpr -> Err Type
inferExp env x = case x of

    -- Boolean expressions
    OrOp e1 e2 (line,_) -> inferBool env line e1 e2
    AndOp e1 e2 (line,_) -> inferBool env line e1 e2
    NotOp e (line,_)-> do
      typ <- inferExp env e
      if typ == BasType BasicType_bool
        then Ok $ BasType BasicType_bool
        else throwError line True $ "Type of (" ++ printTree e ++
                                ") expected (bool) but found (" ++ printTree typ ++ ")"

    -- Arithmetic expression
    AddOp e1 e2 (line,_)-> inferNumericBin env line e1 e2 
    SubOp e1 e2 (line,_)-> inferNumericBin env line e1 e2 
    MulOp e1 e2 (line,_)-> inferNumericBin env line e1 e2
    ModOp e1 e2 (line,_) -> do
      typ <- inferExp env e1
      typ' <- inferExp env e2
      if (typ /= BasType BasicType_int)
        then throwError line True $ "Type of (" ++ printTree e1 ++
                                ") expected (int) but found (" ++ printTree typ ++ ")"
        else if (typ' /= BasType BasicType_int)
          then throwError line True $ "Type of (" ++ printTree e2 ++
                                 ") expected (int) but found (" ++ printTree typ' ++ ")"
          else Ok $ BasType BasicType_int
    NegOp e1 (line,_) -> inferNumeric env line e1
    DivOp e1 e2 (line,_) -> inferNumericBin env line e1 e2

    -- Fun Call
    FCall (Call id exp (line,_)) -> do
      (typs) <- lookFun env line id
      checkParam id line exp typs env
      Ok $ BasType BasicType_void

    -- LExpr
    Lexpr lexp -> case lexp of 
      BasLExpr ble -> do
        (typ, b) <- inferBLExpr env 0 ble
        return typ
      PreInc exp (line,_)-> inferNumeric env line (Lexpr exp) 
      PreDecr exp (line,_) -> inferNumeric env line (Lexpr exp)
      PostInc exp (line,_)-> inferNumeric env line (Lexpr exp)
      PostDecr exp (line,_) -> inferNumeric env line (Lexpr exp)

    -- Confronti
    EqOp e1 e2 (line,_) -> checkComp env line e1 e2
    NeqOp e1 e2 (line,_) -> checkComp env line e1 e2
    LtOp e1 e2 (line,_) -> checkComp env line e1 e2
    LtEOp e1 e2 (line,_) -> checkComp env line e1 e2
    GtOp e1 e2 (line,_) -> checkComp env line e1 e2
    GtEOp e1 e2 (line,_) -> checkComp env line e1 e2

    -- Ref
    RefOp e (line,_) -> do
      typ <- case e of 
        BasLExpr ble -> do
          (typ, b) <- inferBLExpr env line ble
          return typ
        PreInc exp _ -> inferNumeric env line (Lexpr exp) 
        PreDecr exp _ -> inferExp env (Lexpr exp)
        PostInc exp _ -> inferExp env (Lexpr exp)
        PostDecr exp _ -> inferExp env (Lexpr exp)
      return $ CompType $ Pointer typ

    -- Basic Type
    Int e -> return (BasType BasicType_int)
    Char e -> return (BasType BasicType_char)
    Real e -> return (BasType BasicType_float)
    Bool e -> return (BasType BasicType_bool)
    String e -> return (BasType BasicType_string)
    
-- Restituisco il tipo di un espressione complessa ( es. [1,2,3] )
inferComplExp :: Env -> Int -> ComplexRExpr -> Err Type
inferComplExp env line (Array (x:xs)) = do
  typ <- case x of
          (Simple e) -> inferExp env e
          _ -> inferComplExp env line x
  mapM_ (inferListExp typ) xs
  return (CompType (ArrDef typ $ Int $ 1 + toInteger(length xs) ) )
    where
      inferListExp t1 exp = do 
        t2 <- case x of
          (Simple e) -> inferExp env e
          _ -> inferComplExp env line x
        if t1 == t2
           then return t1
           else throwError line True $ "(" ++ printTree x ++ ") has type (" ++ printTree t1
                                   ++ ") but (" ++ printTree exp
                                   ++ ") has type (" ++ printTree t2 ++ ")"

-- Restituisco il tipo di un espressione booleana
inferBool :: Env -> Int -> RExpr -> RExpr -> Err Type
inferBool env line e1 e2 = do t1 <- inferExp env e1
                              t2 <- inferExp env e2
                              if (t1 == (BasType BasicType_bool)) && (t2 == (BasType BasicType_bool))
                                then return (BasType BasicType_bool)
                                else throwError line True $ "(" ++ printTree e1 ++ ") has type (" ++ printTree t1
                                   ++ ") but (" ++ printTree e2
                                   ++ ") has type (" ++ printTree t2 ++ ")"

-- Controllo che il tipo di un operazione sia numerico
inferNumeric :: Env -> Int -> RExpr -> Err Type
inferNumeric env line exp = do BasType typ <- inferExp env exp
                               if (elem typ [BasicType_int, BasicType_float]) then return $ BasType typ
                               else throwError line True $ "Type of expression (" ++ printTree exp ++ ") expected numeric but found (" 
                                                  ++ printTree typ ++ ")" 

-- Restituisco il tipo di un operazione binaria numerica
inferNumericBin :: Env -> Int -> RExpr -> RExpr -> Err Type
inferNumericBin env line e1 e2 = do 
  t1 <- inferExp env e1
  t2 <- inferExp env e2
  if not (elem t1 [BasType BasicType_int, BasType BasicType_float]) 
    then throwError line True $ "Type of expression (" ++ printTree e1 ++ ") expected numeric but found (" 
                     ++ printTree t1 ++ ")"
    else 
      if not (elem t2 [BasType BasicType_int, BasType BasicType_float]) 
        then throwError line True $ "Type of expression (" ++ printTree e2 ++ ") expected numeric but found (" 
                     ++ printTree t2 ++ ")"
        else 
          if t1 == BasType BasicType_float || t2 == BasType BasicType_float
            then return (BasType BasicType_float)
            else return (BasType BasicType_int)

-- Restituisco il tipo di una l-expression
inferBLExpr :: Env -> Int -> BLExpr -> Err (Type, Bool)
inferBLExpr env line exp = inferLExprAux env exp
  where
    inferLExprAux env bexp = case bexp of
      Id id -> lookVar env line id
      (Deref blexp (line,_)) -> do
        (typ, b) <- inferLExprAux env blexp
        typ' <- deref line typ
        return (typ', b)
      (ArrayEl blexp exp (line,_)) -> do
        typ <- inferExp env exp
        if (typ /= BasType BasicType_int)
          then throwError line True $ "Type of (" ++ printTree exp ++
                                 ") expected (int) but found (" ++ printTree typ ++ ")"
          else do
            (typ, b) <- inferLExprAux env blexp
            typ' <- deArr line typ
            return (typ', b)

    deArr line (CompType (ArrNoDef x)) = Ok x
    deArr line (CompType (ArrDef x exp1)) = do
      typ <- inferExp env exp1
      if (typ /= BasType BasicType_int)
        then throwError line True $ "Type of (" ++ printTree exp1 ++
                          ") expected (int) but found (" ++ printTree typ ++ ")"
        else Ok $ BasType BasicType_int
    deArr line e = throwError line True $ "Expected (" ++ printTree (CompType (ArrNoDef e)) ++
                          ") but found (" ++ printTree e ++ ")"

    deref line (CompType (Pointer x)) = Ok x
    deref line e = throwError line True $ "Expected (" ++ printTree (CompType (Pointer e)) ++
                          ") but found (" ++ printTree e ++ ")"

-- Controllo tipo confronto
checkComp :: Env -> Int -> RExpr -> RExpr -> Err Type
checkComp env line e1 e2 = do
  typ <- inferExp env e1
  checkExp env line typ (Simple e2)
  Ok (BasType BasicType_bool)
  
  
  
  
-- Errori
throwError :: Int -> Bool -> String -> Err a
throwError l b s = 
  if b == True
    then fail $ "line " ++ show l ++ ": TYPE ERROR: " ++ s ++ "."
    else fail $ "line " ++ show l ++ ": STATIC ERROR: " ++ s ++ "."
