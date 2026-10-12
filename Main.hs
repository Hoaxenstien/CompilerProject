module Main where

import System.Environment (getArgs)
import qualified N1
import qualified N1Passes
import qualified C0Passes
import qualified X86bPasses
import qualified X86b
import qualified Parsers.NiParser as NiParser
import CompilerPasses

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-a", filename] -> runCompiler filename
        _ -> putStrLn "Usage: stack run -- -a <filename.ni>"

runCompiler :: String -> IO ()
runCompiler filename = do
    putStrLn $ "Compiling: " ++ filename
    
    -- Step 1: Parse
    parseResult <- NiParser.niParseFile filename
    case parseResult of
        Left err -> putStrLn $ "Parse error:\n" ++ err
        Right ast -> do
            putStrLn " Parsing successful"
            
            -- Step 2: Uniquify
            let uResult = N1Passes.uniquify ast
            case N1Passes.getResult uResult of
                Left err -> putStrLn $ "Uniquify error:\n" ++ err
                Right uast -> do
                    putStrLn " Uniquify successful"
                    
                    -- Step 3: Remove Complex Operands
                    let rcoState = 0
                        rcoResult = N1Passes.passRemoveComplexOperas (CState rcoState (Right uast))
                    case N1Passes.getResult rcoResult of
                        Left err -> putStrLn $ "RCO error:\n" ++ err
                        Right rcoast -> do
                            putStrLn " Remove Complex Operands successful"
                            
                            -- Step 4: Explicate Control
                            case C0Passes.explicateControl rcoast of
                                Left err -> putStrLn $ "Explicate Control error:\n" ++ err
                                Right ecPass -> do
                                    putStrLn " Explicate Control successful"
                                    let c0Prog = C0Passes.prog ecPass
                                        locals = C0Passes.locals ecPass
                                    
                                    -- Step 5: Instruction Selection
                                    case X86bPasses.instructionSelection c0Prog locals of
                                        Left err -> putStrLn $ "Instruction Selection error:\n" ++ err
                                        Right siPass -> do
                                            putStrLn " Instruction Selection successful"
                                            let (X86bPasses.SIPass (x86Prog, _)) = siPass
                                            
                                            -- Step 6: Print the x86 assembly
                                            putStrLn "\n=== Generated x86 Assembly ==="
                                            putStrLn $ show x86Prog
                                            
                                            -- TODO: Add Assign Homes and Patch passes here
                                            
                                            putStrLn "\n Compilation complete"