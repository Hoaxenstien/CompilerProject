module N1Passes where

import N1
import Env
import CompilerPasses

-- ============================================
-- UNIQUIFY PASS
-- ============================================

data UniquifyState = UState (Env String) Integer

type UniquifyResult = CompilerResult UniquifyState Exp

uniquify :: N1 -> CompilerResult UniquifyState N1
uniquify (Program exp) =
  case uniquifyExp exp (UState Env.makeEnv 0) of
    CState state (Right exp') -> CState state $ Right $ Program exp'
    CState state (Left msg) -> CState state $ Left msg

uniquifyExp :: Exp -> UniquifyState -> UniquifyResult

-- Base cases (already provided)
uniquifyExp v@(Int _) state = CState state $ Right v
uniquifyExp r@Read state = CState state $ Right r

uniquifyExp (Negate exp) state =
  case uniquifyExp exp state of
    CState state' (Right res) -> CState state' $ Right $ Negate res
    err -> err

-- ADDED: Uniquify both operands, threading the state through left to right
uniquifyExp (Add x y) state =
  case uniquifyExp x state of
    CState state' (Right x') ->
      case uniquifyExp y state' of
        CState state'' (Right y') -> CState state'' $ Right $ Add x' y'
        err -> err
    err -> err

-- ADDED: Look up variable in environment to get its unique renamed version
uniquifyExp (Var sym) (UState env count) =
  case Env.lookup sym env of
    Right newName -> CState (UState env count) $ Right $ Var newName
    Left msg -> CState (UState env count) $ Left msg

-- ADDED: Generate fresh name (sym++counter), uniquify expr, extend env with binding, uniquify body
uniquifyExp (Let sym expr body) (UState env count) =
  let freshName = sym ++ show count
      newCount = count + 1
  in
    case uniquifyExp expr (UState env newCount) of
      CState (UState _ count') (Right expr') ->
        let newEnv = extendEnv sym freshName env
        in
          case uniquifyExp body (UState newEnv count') of
            CState state' (Right body') ->
              CState state' $ Right $ Let freshName expr' body'
            err -> err
      err -> err

-- ============================================
-- REMOVE COMPLEX OPERA* PASS
-- ============================================

type RCOState = Integer
type RCOResult = CompilerResult RCOState Exp
type AtmResult = CompilerResult RCOState (Exp, [(String, Exp)])

passRemoveComplexOperas :: CompilerResult RCOState N1 -> CompilerResult RCOState N1
passRemoveComplexOperas (CState symCount (Right (Program expr))) =
    case rcoExp (CState symCount (Right expr)) of
      CState symCount' (Right exp') -> CState symCount' $ Right $ Program exp'
      CState symCount' (Left msg) -> CState symCount' $ Left msg
passRemoveComplexOperas (CState symCount (Left msg)) = CState symCount (Left msg)

rcoExp :: RCOResult -> RCOResult  
rcoExp atm@(CState _ (Right Read)) = atm
rcoExp atm@(CState _ (Right (Int _))) = atm
rcoExp atm@(CState _ (Right (Var _))) = atm

-- ADDED: Process both expr and body recursively
rcoExp (CState state (Right (Let sym expr body))) =
  case rcoExp (CState state (Right expr)) of
    CState state' (Right expr') ->
      case rcoExp (CState state' (Right body)) of
        CState state'' (Right body') ->
          CState state'' $ Right $ Let sym expr' body'
        err -> err
    err -> err

-- ADDED: Make operand atomic using rcoAtm, then wrap result in any generated Let bindings
rcoExp (CState state (Right (Negate expr))) =
  case rcoAtm (CState state (Right (expr, []))) of
    CState state' (Right (atomicExpr, bindings)) ->
      let negateExpr = Negate atomicExpr
          result = foldr (\(name, exp) body -> Let name exp body) negateExpr bindings
      in CState state' $ Right result
    err -> CState state (Left $ extractError err)

-- ADDED: Make both operands atomic, combine their bindings, wrap Add in all generated Lets
rcoExp (CState state (Right (Add x y))) =
  case rcoAtm (CState state (Right (x, []))) of
    CState state' (Right (atomicX, bindingsX)) ->
      case rcoAtm (CState state' (Right (y, []))) of
        CState state'' (Right (atomicY, bindingsY)) ->
          let allBindings = bindingsX ++ bindingsY
              addExpr = Add atomicX atomicY
              result = foldr (\(name, exp) body -> Let name exp body) addExpr allBindings
          in CState state'' $ Right result
        err -> CState state (Left $ extractError err)
    err -> CState state (Left $ extractError err)

rcoExp (CState _ (Left msg)) = CState 0 (Left msg)

-- ADDED: Helper to extract error messages
extractError :: AtmResult -> String
extractError (CState _ (Left msg)) = msg
extractError _ = "Unknown error"

rcoAtm :: AtmResult -> AtmResult
rcoAtm res@(CState _ (Right (Int _, _))) = res 
rcoAtm res@(CState _ (Right (Read, _))) = res
rcoAtm res@(CState _ (Right (Var _, _))) = res

-- ADDED: Make operand atomic, generate fresh temp variable, add binding for this Negate
rcoAtm (CState symCount (Right (Negate expr, lst))) =
  case rcoAtm (CState symCount (Right (expr, lst))) of
    CState symCount' (Right (atomicExpr, bindings)) ->
      let freshName = "s" ++ show symCount'
          negateExpr = Negate atomicExpr
          newBindings = bindings ++ [(freshName, negateExpr)]
      in CState (symCount' + 1) $ Right (Var freshName, newBindings)
    err -> err

-- ADDED: Make both operands atomic, generate fresh temp for Add result, accumulate all bindings
rcoAtm (CState symCount (Right (Add e1 e2, lst))) =
  case rcoAtm (CState symCount (Right (e1, lst))) of
    CState symCount' (Right (atomic1, bindings1)) ->
      case rcoAtm (CState symCount' (Right (e2, []))) of
        CState symCount'' (Right (atomic2, bindings2)) ->
          let freshName = "s" ++ show symCount''
              addExpr = Add atomic1 atomic2
              allBindings = bindings1 ++ bindings2 ++ [(freshName, addExpr)]
          in CState (symCount'' + 1) $ Right (Var freshName, allBindings)
        err -> err
    err -> err

-- ADDED: Make expr and body atomic, generate fresh temp for Let result, accumulate all bindings
rcoAtm (CState symCount (Right (Let sym expr body, lst))) =
  case rcoAtm (CState symCount (Right (expr, lst))) of
    CState symCount' (Right (atomicExpr, bindings1)) ->
      case rcoAtm (CState symCount' (Right (body, []))) of
        CState symCount'' (Right (atomicBody, bindings2)) ->
          let letExpr = Let sym atomicExpr atomicBody
              freshName = "s" ++ show symCount''
              allBindings = bindings1 ++ bindings2 ++ [(freshName, letExpr)]
          in CState (symCount'' + 1) $ Right (Var freshName, allBindings)
        err -> err
    err -> err