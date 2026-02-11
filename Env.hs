module Env where

import Data.List as List

-- Our Env(ronment) is an association list of name/value bindings
newtype Env a = Env [(String,a)] deriving (Eq,Show)

-- makeEnv creates a new empty environment
makeEnv :: Env a
makeEnv = Env []


{--
  extendEnv takes a symbol (String) and a value (a) and
  an Env, and returns a new Env with
  (sym,value) as the first binding followed by the old Env
--}
extendEnv :: String -> a -> Env a -> Env a
extendEnv sym val (Env oldEnv) =
  Env $ (sym, val) : oldEnv


{--
  lookup takes a String and a Env and looks
  up the symbol in the Environment, returning
  Left "<error msg>" if it fails or Right <val>
  with the discovered value. 
--}
lookup :: String -> Env a -> Either String a
lookup sym (Env env) =
  case List.lookup sym env of
    Just v -> Right v
    Nothing -> Left ("Symbol '" ++ sym ++ "' not found")