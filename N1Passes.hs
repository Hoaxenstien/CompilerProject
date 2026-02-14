module N1Passes where

import N1
import Env
import CompilerPasses

-- ============================================
-- UNIQUIFY PASS
-- ============================================

data UniquifyState = UState (Env String) Integer

type UniquifyResult = CompilerResult UniquifyState Exp

-- Helper function to extract result from CompilerResult (generic version)
getResult :: CompilerResult a b -> Result b
getResult (CState _ (Right p)) = Right p
getResult (CState _ err) = err

uniquify :: Program -> CompilerResult UniquifyState Program
uniquify (Program exp) =
  case uniquifyExp exp (UState Env.makeEnv 0) of
    CState state (Right exp') -> CState state $ Right $ Program exp'
    CState state (Left msg) -> CState state $ Left msg

uniquifyExp :: Exp -> UniquifyState -> UniquifyResult

-- Base cases
uniquifyExp v@(Int _) state = CState state $ Right v
uniquifyExp r@Read state = CState state $ Right r

uniquifyExp (Negate exp) state =
  case uniquifyExp exp state of
    CState state' (Right res) -> CState state' $ Right $ Negate res
    err -> err

-- Uniquify both operands, threading the state through left to right
uniquifyExp (Add x y) state =
  case uniquifyExp x state of
    CState state' (Right x') ->
      case uniquifyExp y state' of
        CState state'' (Right y') -> CState state'' $ Right $ Add x' y'
        err -> err
    err -> err

-- Look up variable in environment to get its unique renamed version
uniquifyExp (Var sym) (UState env count) =
  case Env.lookup sym env of
    Right newName -> CState (UState env count) $ Right $ Var newName
    Left msg -> CState (UState env count) $ Left msg

-- Generate fresh name, uniquify expr, extend env with binding, uniquify body
uniquifyExp (Let sym expr body) (UState env count) =
  let freshName = "s" ++ show count
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

passRemoveComplexOperas :: CompilerResult RCOState Program -> CompilerResult RCOState Program
passRemoveComplexOperas (CState symCount (Right (Program expr))) =
    case rcoExp (CState symCount (Right expr)) of
      CState symCount' (Right exp') -> CState symCount' $ Right $ Program exp'
      CState symCount' (Left msg) -> CState symCount' $ Left msg
passRemoveComplexOperas (CState symCount (Left msg)) = CState symCount (Left msg)

rcoExp :: RCOResult -> RCOResult  
-- Atomic expressions stay as-is
rcoExp atm@(CState _ (Right Read)) = atm
rcoExp atm@(CState _ (Right (Int _))) = atm
rcoExp atm@(CState _ (Right (Var _))) = atm

-- Process both expr and body recursively
rcoExp (CState state (Right (Let sym expr body))) =
  case rcoExp (CState state (Right expr)) of
    CState state' (Right expr') ->
      case rcoExp (CState state' (Right body)) of
        CState state'' (Right body') ->
          CState state'' $ Right $ Let sym expr' body'
        err -> err
    err -> err

-- Make operand atomic using rcoAtm, then wrap result in any generated Let bindings
rcoExp (CState state (Right (Negate expr))) =
  case rcoAtm (CState state (Right (expr, []))) of
    CState state' (Right (atomicExpr, bindings)) ->
      let negateExpr = Negate atomicExpr
          result = foldr (\(name, exp) body -> Let name exp body) negateExpr bindings
      in CState state' $ Right result
    err -> CState state (Left $ extractError err)

-- Make both operands atomic, combine their bindings, wrap Add in all generated Lets
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

-- Helper to extract error messages
extractError :: AtmResult -> String
extractError (CState _ (Left msg)) = msg
extractError _ = "Unknown error"

-- rcoAtm processes expressions that need to become atomic
-- It returns a pair: (atomic expression, list of bindings to add)
rcoAtm :: AtmResult -> AtmResult

-- Already atomic - no change needed
rcoAtm res@(CState _ (Right (Int _, _))) = res 
rcoAtm res@(CState _ (Right (Read, _))) = res
rcoAtm res@(CState _ (Right (Var _, _))) = res

-- Negate: make operand atomic, generate fresh temp variable, add binding for this Negate
rcoAtm (CState symCount (Right (Negate expr, lst))) =
  case rcoAtm (CState symCount (Right (expr, []))) of
    CState symCount' (Right (atomicExpr, bindings)) ->
      let freshName = "s" ++ show symCount'
          negateExpr = Negate atomicExpr
          -- Combine existing bindings with the new bindings from the subexpression
          newBindings = lst ++ bindings ++ [(freshName, negateExpr)]
      in CState (symCount' + 1) $ Right (Var freshName, newBindings)
    err -> err

-- Add: make both operands atomic, generate fresh temp for Add result
rcoAtm (CState symCount (Right (Add e1 e2, lst))) =
  case rcoAtm (CState symCount (Right (e1, []))) of
    CState symCount' (Right (atomic1, bindings1)) ->
      case rcoAtm (CState symCount' (Right (e2, []))) of
        CState symCount'' (Right (atomic2, bindings2)) ->
          let freshName = "s" ++ show symCount''
              addExpr = Add atomic1 atomic2
              -- Combine all bindings: incoming lst + e1 bindings + e2 bindings + new Add binding
              allBindings = lst ++ bindings1 ++ bindings2 ++ [(freshName, addExpr)]
          in CState (symCount'' + 1) $ Right (Var freshName, allBindings)
        err -> err
    err -> err

-- Let: recursively process expr and body, but DON'T lift the Let itself
-- Let expressions are already in a good form and should remain
rcoAtm (CState symCount (Right (Let sym expr body, lst))) =
  case rcoExp (CState symCount (Right expr)) of
    CState symCount' (Right expr') ->
      case rcoExp (CState symCount' (Right body)) of
        CState symCount'' (Right body') ->
          let letExpr = Let sym expr' body'
              -- If Let appears in atom position, we need to lift it
              freshName = "s" ++ show symCount''
              newBindings = lst ++ [(freshName, letExpr)]
          in CState (symCount'' + 1) $ Right (Var freshName, newBindings)
        err -> CState symCount (Left $ extractErrorFromRCO err)
    err -> CState symCount (Left $ extractErrorFromRCO err)

-- Helper to extract error from RCOResult
extractErrorFromRCO :: RCOResult -> String
extractErrorFromRCO (CState _ (Left msg)) = msg
extractErrorFromRCO _ = "Unknown error"