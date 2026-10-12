module X86bPasses where

import qualified X86b as X86
import qualified C0 as C
import CompilerPasses
import Data.Int (Int64)

newtype SIPass = SIPass (X86.X86b, [String]) deriving (Eq, Show)

-- | Top-level instruction selection.
--   Translates a C0 program into an X86b program.
instructionSelection :: C.Program -> [String] -> Result SIPass
instructionSelection (C.Program blocks) locals =
    case blocks of
        [(label, tail)] ->
            let instrs = selectTail tail
                block = X86.Block instrs
                x86Program = X86.Program [(X86.Label "start", block)]
            in Right $ SIPass (x86Program, locals)
        _ -> Left "Multiple blocks not supported yet"

-- | Convert a C0 Atom to an X86b Arg.
atmToArg :: C.Atom -> X86.Arg
atmToArg (C.Int i) = X86.Imm (fromIntegral i)
atmToArg (C.Var v) = X86.Var v

-- | Select instructions for a C0 expression, storing the result in @dest@.
selectStmt :: C.Exp -> X86.Arg -> [X86.Instr]
selectStmt (C.Atm atm) dest =
    [X86.Movq (atmToArg atm) dest]

selectStmt C.Read dest =
    [ X86.Callq (X86.Label "read_int") 0
    , X86.Movq  (X86.Reg X86.RAX) dest
    ]

selectStmt (C.Sub atm) dest =
    [ X86.Movq (atmToArg atm) dest
    , X86.Negq dest
    ]

selectStmt (C.Add atm1 atm2) dest =
    [ X86.Movq (atmToArg atm1) dest
    , X86.Addq (atmToArg atm2) dest
    ]

-- | Select instructions for a C0 tail (end of a basic block).
selectTail :: C.Tail -> [X86.Instr]
selectTail (C.Return expr) =
    case expr of
        C.Atm atm ->
            [ X86.Movq (atmToArg atm) (X86.Reg X86.RAX)
            , X86.Jmp  (X86.Label "conclusion")
            ]
        C.Read ->
            [ X86.Callq (X86.Label "read_int") 0
            , X86.Jmp   (X86.Label "conclusion")
            ]
        C.Sub atm ->
            [ X86.Movq (atmToArg atm) (X86.Reg X86.RAX)
            , X86.Negq (X86.Reg X86.RAX)
            , X86.Jmp  (X86.Label "conclusion")
            ]
        C.Add atm1 atm2 ->
            [ X86.Movq (atmToArg atm1) (X86.Reg X86.RAX)
            , X86.Addq (atmToArg atm2) (X86.Reg X86.RAX)
            , X86.Jmp  (X86.Label "conclusion")
            ]

selectTail (C.Seq stmt tail) =
    let (C.Assign var expr) = stmt
        instrs = selectStmt expr (X86.Var var)
    in instrs ++ selectTail tail
