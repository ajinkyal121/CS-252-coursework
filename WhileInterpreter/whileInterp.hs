{-
  Name: Ajinkya Lakade
  Class: CS 252
  Assigment: HW2
  Date: 11 MAR 2023
  Description: Interpreters for While semantics
-}


module WhileInterp (
  Expression(..),
  Binop(..),
  Value(..),
  testProgram,
  run
) where

import Data.Map (Map)
import qualified Data.Map as Map

-- We represent variables as strings.
type Variable = String

-- The store is an associative map from variables to values.
-- (The store roughly corresponds with the heap in a language like Java).
type Store = Map Variable Value

data Expression =
    Var Variable                            -- x
  | Val Value                               -- v
  | Assign Variable Expression              -- x := e
  | Sequence Expression Expression          -- e1; e2
  | Op Binop Expression Expression
  | If Expression Expression Expression     -- if e1 then e2 else e3
  | While Expression Expression             -- while (e1) e2
  | And Expression Expression
  | Or Expression Expression
  | Not Expression
  deriving (Show)

data Binop =
    Plus     -- +  :: Int  -> Int  -> Int
  | Minus    -- -  :: Int  -> Int  -> Int
  | Times    -- *  :: Int  -> Int  -> Int
  | Divide   -- /  :: Int  -> Int  -> Int
  | Gt       -- >  :: Int -> Int -> Bool
  | Ge       -- >= :: Int -> Int -> Bool
  | Lt       -- <  :: Int -> Int -> Bool
  | Le       -- <= :: Int -> Int -> Bool
  deriving (Show)

data Value =
    IntVal Int
  | BoolVal Bool
  deriving (Show)


-- This function will be useful for defining binary operations.
-- The first case is done for you.
-- Be sure to explicitly check for a divide by 0 and throw an error.
applyOp :: Binop -> Value -> Value -> Value
applyOp Plus (IntVal i) (IntVal j) = IntVal $ i + j
applyOp Minus (IntVal i) (IntVal j) = IntVal $ i - j
applyOp Times (IntVal i) (IntVal j) = IntVal $ i * j
applyOp Divide (IntVal i) (IntVal 0) = error "Division by 0 not supported"
applyOp Divide (IntVal i) (IntVal j) = IntVal $ i `div` j
applyOp Gt (IntVal i) (IntVal j) = BoolVal $ i > j
applyOp Ge (IntVal i) (IntVal j) = BoolVal $ i >= j
applyOp Lt (IntVal i) (IntVal j) = BoolVal $ i < j
applyOp Le (IntVal i) (IntVal j) = BoolVal $ i <= j
applyOp _ _ _ = error "Binary operations not supported for Boolean Values"


-- Implement this function according to the specified semantics
evaluate :: Expression -> Store -> (Value, Store)
evaluate (Op o e1 e2) s =
  let (v1,s1) = evaluate e1 s
      (v2,s') = evaluate e2 s1
  in (applyOp o v1 v2, s')

-- if true
evaluate (If (Val (BoolVal True)) e2 e3) s = evaluate e2 s

-- if false
evaluate (If (Val (BoolVal False)) e2 e3) s = evaluate e3 s

evaluate (If (Val _) _ _) _ = error "Intval not supported"

evaluate (If e1 e2 e3) s =
  let (e1', s') = evaluate e1 s
   in evaluate (If (Val e1') e2 e3) s'

evaluate (Var x) s = case (Map.lookup x s) of
                      (Just v) -> (v, s)
                      _ -> error "Value not present"

evaluate (Val v) s = (v, s)

-- assign
evaluate (Assign x e) s =
  let (v, s') = evaluate e s
   in (v, Map.insert x v s')

-- sequence
evaluate (Sequence e1 e2) s =
  let (v1, s') = evaluate e1 s
   in evaluate e2 s'

-- while
evaluate (While e1 e2) s = evaluate (If e1 (Sequence e2 (While e1 e2)) (Val (BoolVal False))) s

-- Not
evaluate (Not e) s = evaluate (If e (Val (BoolVal False)) (Val (BoolVal True))) s

-- Or
evaluate (Or e1 e2) s = evaluate (If e1 (Val (BoolVal True)) (If e2 (Val (BoolVal True)) (Val (BoolVal False)))) s

-- And
evaluate (And e1 e2) s = evaluate (If e1 (If e2 (Val (BoolVal True)) (Val (BoolVal False))) (Val (BoolVal False))) s

-- Evaluates a program with an initially empty state
run :: Expression -> (Value, Store)
run prog = evaluate prog Map.empty

-- The same as run, but only returns the Store
testProgram :: Expression -> Store
testProgram prog = snd $ run prog

