module C0 where

import Data.Int (Int64)

-- C0 Language Definition
-- This is the intermediate representation after explicate control
-- Programs consist of labeled blocks that contain sequences of statements

-- A C0 program is a collection of labeled basic blocks
data Program = Program [(String, Tail)]
  deriving (Eq, Show)

-- Atoms are simple values (no operations)
data Atom 
  = Int Int64
  | Var String
  deriving (Eq, Show)

-- Expressions can be atoms or operations
data Exp
  = Atm Atom
  | Read
  | Negate Atom
  | Add Atom Atom
  deriving (Eq, Show)

-- Statements are assignments
data Stmt
  = Assign String Exp
  deriving (Eq, Show)

-- Tails represent the end of a basic block
-- Either return a value or continue with more statements
data Tail
  = Return Exp
  | Seq Stmt Tail
  deriving (Eq, Show)