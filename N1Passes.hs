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

-- TODO: Implement these three cases
uniquifyExp (Add x y) state = undefined
uniquifyExp (Var sym) state = undefined
uniquifyExp (Let sym exp body) state = undefined

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

rcoExp :: RCOResult -> RCOResult  
rcoExp atm@(CState _ (Right Read)) = atm
rcoExp atm@(CState _ (Right (Int _))) = atm
rcoExp atm@(CState _ (Right (Var _))) = atm

-- TODO: Implement these cases
rcoExp (CState state (Right (Let sym expr body))) = undefined
rcoExp (CState state (Right (Negate expr))) = undefined
rcoExp (CState state (Right (Add x y))) = undefined
rcoExp (CState _ (Left msg)) = CState 0 (Left msg)

rcoAtm :: AtmResult -> AtmResult
rcoAtm res@(CState _ (Right (Int _, _))) = res 
rcoAtm res@(CState _ (Right (Read, _))) = res
rcoAtm res@(CState _ (Right (Var _, _))) = res

-- TODO: Implement these cases
rcoAtm (CState symCount (Right (Negate expr, lst))) = undefined
rcoAtm (CState symCount (Right (Add e1 e2, lst))) = undefined
rcoAtm (CState symCount (Right (Let sym expr body, lst))) = undefined