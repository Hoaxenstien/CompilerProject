module X86bPasses where

import qualified X86b as X86
import qualified C0 as C
import CompilerPasses
import Data.Int (Int64)

newtype SIPass = SIPass (X86.X86b, [String]) deriving (Eq, Show)

instructionSelection :: C.Program -> [String] -> Result SIPass
instructionSelection (C.Program blocks) locals =
    case blocks of
        [(label, tail)] ->
            let instrs = selectTail tail
                block = X86.Block instrs
                x86Program = X86.Program [(X86.Label "start", block)]
            in Right $ SIPass (x86Program, locals)
        _ -> Left "Multiple blocks not supported yet"

-- Convert C0 Atom to X86b Arg
atmToArg :: C.Atom -> X86.Arg
atmToArg (C.Int i) = X86.Imm (fromIntegral i)
atmToArg (C.Var v) = X86.Var v

-- Select instructions for a statement, storing result in dest
selectStmt :: C.Exp -> X86.Arg -> [X86.Instr]
-- Atomic expressions
selectStmt (C.Atm atm) dest =
    [X86.Movq (atmToArg atm) dest]
    
-- Read operation
selectStmt C.Read dest =
    [ X86.Callq (X86.Label "read_int") 0  -- arity 0 for read_int
    , X86.Movq (X86.Reg X86.RAX) dest
    ]
    
-- Negation (using C.Sub since C0 uses Sub for unary minus)
selectStmt (C.Sub atm) dest =
    [ X86.Movq (atmToArg atm) dest
    , X86.Negq dest
    ]
    
-- Addition
selectStmt (C.Add atm1 atm2) dest =
    [ X86.Movq (atmToArg atm1) dest
    , X86.Addq (atmToArg atm2) dest
    ]

-- Select instructions for a tail
selectTail :: C.Tail -> [X86.Instr]
-- Return statement
selectTail (C.Return exp) =
    case exp of
        C.Atm atm ->
            [ X86.Movq (atmToArg atm) (X86.Reg X86.RAX)
            , X86.Jmp (X86.Label "conclusion")
            ]
        C.Read ->
            [ X86.Callq (X86.Label "read_int") 0
            , X86.Jmp (X86.Label "conclusion")
            ]
        C.Sub atm ->
            [ X86.Movq (atmToArg atm) (X86.Reg X86.RAX)
            , X86.Negq (X86.Reg X86.RAX)
            , X86.Jmp (X86.Label "conclusion")
            ]
        C.Add atm1 atm2 ->
            [ X86.Movq (atmToArg atm1) (X86.Reg X86.RAX)
            , X86.Addq (atmToArg atm2) (X86.Reg X86.RAX)
            , X86.Jmp (X86.Label "conclusion")
            ]
            
-- Sequence of statements
selectTail (C.Seq stmt tail) =
    let (C.Assign var exp) = stmt
        instrs = selectStmt exp (X86.Var var)
    in instrs ++ selectTail tail