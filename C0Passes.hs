module C0Passes where

import N1 as N1
import C0 as C0
import CompilerPasses
import Data.List (nub)

data ECPass = ECPass { prog :: C0.Program, locals :: [String] } deriving (Eq, Show)

explicateControl :: N1.Program -> Result ECPass
explicateControl (N1.Program expr) = 
    case explicateTail expr of
        Right (tail, locals) -> 
            Right $ ECPass (C0.Program [("start", tail)]) (nub locals)
        Left err -> Left err

-- explicateTail handles expressions in tail position
explicateTail :: N1.Exp -> Result (C0.Tail, [String])
explicateTail (N1.Int x) = 
    Right (C0.Return (C0.Atm (C0.Int x)), [])
    
explicateTail (N1.Var x) = 
    Right (C0.Return (C0.Atm (C0.Var x)), [x])
    
explicateTail N1.Read = 
    Right (C0.Return C0.Read, [])
    
explicateTail (N1.Negate e) = 
    case e of
        N1.Int x -> 
            Right (C0.Return (C0.Sub (C0.Int x)), [])
        N1.Var x -> 
            Right (C0.Return (C0.Sub (C0.Var x)), [x])
        _ -> Left "Negate of non-atomic in tail position"
        
explicateTail (N1.Add e1 e2) = 
    case (e1, e2) of
        (N1.Int x, N1.Int y) -> 
            Right (C0.Return (C0.Add (C0.Int x) (C0.Int y)), [])
        (N1.Int x, N1.Var y) -> 
            Right (C0.Return (C0.Add (C0.Int x) (C0.Var y)), [y])
        (N1.Var x, N1.Int y) -> 
            Right (C0.Return (C0.Add (C0.Var x) (C0.Int y)), [x])
        (N1.Var x, N1.Var y) -> 
            Right (C0.Return (C0.Add (C0.Var x) (C0.Var y)), [x, y])
        _ -> Left "Add with non-atomic operands in tail position"
        
explicateTail (N1.Let var rhs body) = 
    case explicateAssign var rhs (C0.Return (C0.Atm (C0.Var var))) of
        Right (assignTail, rhsLocals) ->
            case explicateTail body of
                Right (bodyTail, bodyLocals) ->
                    Right (combineTails assignTail bodyTail, var : rhsLocals ++ bodyLocals)
                Left err -> Left err
        Left err -> Left err

-- explicateAssign handles expressions NOT in tail position
explicateAssign :: String -> N1.Exp -> C0.Tail -> Result (C0.Tail, [String])
explicateAssign sym (N1.Int v) ctail =
    Right (C0.Seq (C0.Assign sym (C0.Atm (C0.Int v))) ctail, [sym])
    
explicateAssign sym (N1.Var v) ctail =
    Right (C0.Seq (C0.Assign sym (C0.Atm (C0.Var v))) ctail, [sym, v])
    
explicateAssign sym N1.Read ctail =
    Right (C0.Seq (C0.Assign sym C0.Read) ctail, [sym])
    
explicateAssign sym (N1.Negate e) ctail =
    case e of
        N1.Int v ->
            Right (C0.Seq (C0.Assign sym (C0.Sub (C0.Int v))) ctail, [sym])
        N1.Var v ->
            Right (C0.Seq (C0.Assign sym (C0.Sub (C0.Var v))) ctail, [sym, v])
        _ -> Left "Complex operand in negation"
        
explicateAssign sym (N1.Add e1 e2) ctail =
    case (e1, e2) of
        (N1.Int v1, N1.Int v2) ->
            Right (C0.Seq (C0.Assign sym (C0.Add (C0.Int v1) (C0.Int v2))) ctail, [sym])
        (N1.Int v1, N1.Var v2) ->
            Right (C0.Seq (C0.Assign sym (C0.Add (C0.Int v1) (C0.Var v2))) ctail, [sym, v2])
        (N1.Var v1, N1.Int v2) ->
            Right (C0.Seq (C0.Assign sym (C0.Add (C0.Var v1) (C0.Int v2))) ctail, [sym, v1])
        (N1.Var v1, N1.Var v2) ->
            Right (C0.Seq (C0.Assign sym (C0.Add (C0.Var v1) (C0.Var v2))) ctail, [sym, v1, v2])
        _ -> Left "Complex addition"

explicateAssign sym (N1.Let var rhs body) ctail =
    case explicateAssign var rhs (C0.Return (C0.Atm (C0.Var var))) of
        Right (letTail, letLocals) ->
            case explicateAssign sym body ctail of
                Right (bodyTail, bodyLocals) ->
                    Right (combineTails letTail bodyTail, sym : letLocals ++ bodyLocals)
                Left err -> Left err
        Left err -> Left err

-- Helper to combine two tails
combineTails :: C0.Tail -> C0.Tail -> C0.Tail
combineTails (C0.Seq stmt tail1) tail2 = C0.Seq stmt (combineTails tail1 tail2)
combineTails (C0.Return _) tail2 = tail2