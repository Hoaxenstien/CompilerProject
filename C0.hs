module C0 where

import Data.Int (Int64)

-- A C0 program is a collection of labeled basic blocks
data Program = Program [(String, Tail)]
  deriving (Eq, Show)

-- Type alias so parser can use C0
type C0 = Program

-- Atoms are simple values (no operations)
data Atom 
  = Int Int64
  | Var String
  deriving (Eq, Show)

-- Expressions can be atoms or operations
-- IMPORTANT: No Negate constructor! Only Sub for unary minus.
data Exp
  = Atm Atom
  | Read
  | Sub Atom        -- Unary minus (renamed from Negate to avoid conflict)
  | Add Atom Atom
  deriving (Eq, Show)

-- Statements are assignments
data Stmt
  = Assign String Exp
  deriving (Eq, Show)

-- Tails represent the end of a basic block
data Tail
  = Return Exp
  | Seq Stmt Tail
  deriving (Eq, Show)