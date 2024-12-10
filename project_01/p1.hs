-- Project 1 EECS 662
-- Dylan A Davis
-- 3047302
-- 10/3/2024
--


{-# LANGUAGE GADTs, FlexibleContexts #-}

-- Imports for Monads
import Control.Monad

-- AST Definition
data TABE where
  TNum :: TABE
  TBool :: TABE
  deriving (Show, Eq)

data ABE where
  Num :: Int -> ABE
  Plus :: ABE -> ABE -> ABE
  Minus :: ABE -> ABE -> ABE
  Mult :: ABE -> ABE -> ABE
  Div :: ABE -> ABE -> ABE
  Boolean :: Bool -> ABE
  And :: ABE -> ABE -> ABE
  Leq :: ABE -> ABE -> ABE
  IsZero :: ABE -> ABE
  If :: ABE -> ABE -> ABE -> ABE
  deriving (Show, Eq)

-- Evaluation Functions
evalM :: ABE -> (Maybe ABE)
evalM (Num n) = Just (Num n)
evalM (Boolean b) = Just (Boolean b)
evalM (Plus e1 e2) = do
    Num n1 <- evalM e1
    Num n2 <- evalM e2
    return $ Num (n1 + n2)
evalM (Minus e1 e2) = do
    Num n1 <- evalM e1
    Num n2 <- evalM e2
    if n1 >= n2
        then return $ Num (n1 - n2)
        else Nothing
evalM (Mult e1 e2) = do
    Num n1 <- evalM e1
    Num n2 <- evalM e2
    return $ Num (n1 * n2)
evalM (Div e1 e2) = do
    Num n1 <- evalM e1
    Num n2 <- evalM e2
    guard (n2 > 0)  -- Changed from n2 /= 0
    return $ Num (n1 `div` n2)
evalM (And e1 e2) = do
    Boolean b1 <- evalM e1
    Boolean b2 <- evalM e2
    return $ Boolean (b1 && b2)
evalM (Leq e1 e2) = do
    Num n1 <- evalM e1
    Num n2 <- evalM e2
    return $ Boolean (n1 <= n2)
evalM (IsZero e) = do
    Num n <- evalM e
    return $ Boolean (n == 0)
evalM (If cond thenExpr elseExpr) = do
    Boolean c <- evalM cond
    if c then evalM thenExpr else evalM elseExpr

-- Type Derivation Function
typeofM :: ABE -> Maybe TABE
typeofM (Num _) = Just TNum
typeofM (Boolean _) = Just TBool
typeofM (Plus e1 e2) = do
    t1 <- typeofM e1
    t2 <- typeofM e2
    guard (t1 == TNum && t2 == TNum)
    return TNum
typeofM (Minus e1 e2) = do
    t1 <- typeofM e1
    t2 <- typeofM e2
    guard (t1 == TNum && t2 == TNum)
    return TNum
typeofM (Mult e1 e2) = do
    t1 <- typeofM e1
    t2 <- typeofM e2
    guard (t1 == TNum && t2 == TNum)
    return TNum
typeofM (Div e1 e2) = do
    t1 <- typeofM e1
    t2 <- typeofM e2
    guard (t1 == TNum && t2 == TNum)
    return TNum
typeofM (And e1 e2) = do
    t1 <- typeofM e1
    t2 <- typeofM e2
    guard (t1 == TBool && t2 == TBool)
    return TBool
typeofM (Leq e1 e2) = do
    t1 <- typeofM e1
    t2 <- typeofM e2
    guard (t1 == TNum && t2 == TNum)
    return TBool
typeofM (IsZero e) = do
    t <- typeofM e
    guard (t == TNum)
    return TBool
typeofM (If cond thenExpr elseExpr) = do
    tCond <- typeofM cond
    tThen <- typeofM thenExpr
    tElse <- typeofM elseExpr
    guard (tCond == TBool && tThen == tElse)
    return tThen

-- Combined interpreter
evalTypeM :: ABE -> Maybe ABE
evalTypeM (Num n) = Just (Num n)
evalTypeM (Boolean b) = Just (Boolean b)
evalTypeM (Plus l r) = do
    Num l' <- evalTypeM l
    Num r' <- evalTypeM r
    return $ Num (l' + r')
evalTypeM (Minus l r) = do
    Num l' <- evalTypeM l
    Num r' <- evalTypeM r
    if l' >= r' then return $ Num (l' - r') else Nothing
evalTypeM (Mult l r) = do
    Num l' <- evalTypeM l
    Num r' <- evalTypeM r
    return $ Num (l' * r')
evalTypeM (Div l r) = do
    Num l' <- evalTypeM l
    Num r' <- evalTypeM r
    if r' /= 0 then return $ Num (l' `div` r') else Nothing
evalTypeM (And l r) = do
    Boolean l' <- evalTypeM l
    Boolean r' <- evalTypeM r
    return $ Boolean (l' && r')
evalTypeM (Leq l r) = do
    Num l' <- evalTypeM l
    Num r' <- evalTypeM r
    return $ Boolean (l' <= r')
evalTypeM (IsZero e) = do
    Num e' <- evalTypeM e
    return $ Boolean (e' == 0)
evalTypeM (If c t e) = do
    Boolean c' <- evalTypeM c
    if c' then evalTypeM t else evalTypeM e

-- Optimizer
optimize :: ABE -> ABE
optimize (Num n) = Num n
optimize (Boolean b) = Boolean b
optimize (Plus e (Num 0)) = optimize e
optimize (Plus (Num 0) e) = optimize e
optimize (Plus a b) = Plus (optimize a) (optimize b)
optimize (If (Boolean True) e _) = optimize e
optimize (If (Boolean False) _ e) = optimize e
optimize (If cond a b) = If (optimize cond) (optimize a) (optimize b)
optimize (Mult a b) = Mult (optimize a) (optimize b)
optimize (Leq a b) = Leq (optimize a) (optimize b)
optimize (IsZero a) = IsZero (optimize a)
optimize e = e

evalOptM :: ABE -> Maybe ABE
evalOptM x = evalM (optimize x)

main :: IO ()
main = do
    -- evalM Tests
    print $ evalM (Plus (Num 7) (Num 3))                -- 10
    print $ evalM (Minus (Num 10) (Num 4))              -- 6
    print $ evalM (Mult (Num 6) (Num 7))                -- 42
    print $ evalM (Div (Num 24) (Num 4))                 -- 6
    print $ evalM (Div (Num 5) (Num 0))                  -- Nothing (Division by zero)
    print $ evalM (And (Boolean True) (Boolean True))    -- True
    print $ evalM (Leq (Num 5) (Num 7))                  -- True
    print $ evalM (If (Boolean True) (Num 8) (Num 3))   -- 8
    print $ evalM (If (Boolean False) (Num 8) (Num 3))  -- 3
    print $ evalM (IsZero (Num 0))                       -- True
    print $ evalM (IsZero (Num 9))                       -- False

    -- typeofM Tests
    print $ typeofM (Num 9)                               -- Just TNum
    print $ typeofM (Boolean True)                        -- Just TBool
    print $ typeofM (Plus (Num 4) (Num 5))               -- Just TNum
    print $ typeofM (Minus (Num 8) (Num 3))              -- Just TNum
    print $ typeofM (Mult (Num 2) (Num 6))               -- Just TNum
    print $ typeofM (Div (Num 18) (Num 3))               -- Just TNum
    print $ typeofM (And (Boolean False) (Boolean True)) -- Just TBool
    print $ typeofM (Leq (Num 6) (Num 12))               -- Just TBool
    print $ typeofM (If (Boolean True) (Num 2) (Num 5))  -- Just TNum
    print $ typeofM (IsZero (Num 4))                     -- Just TBool

    -- evalTypeM Tests
    print $ evalTypeM (Plus (Num 8) (Num 12))            -- Just (Num 20)
    print $ evalTypeM (And (Boolean True) (Boolean False)) -- Just (Boolean False)
    print $ evalTypeM (Minus (Num 10) (Num 5))           -- Just (Num 5)
    print $ evalTypeM (If (Boolean True) (Num 6) (Num 4)) -- Just (Num 6)
    print $ evalTypeM (If (Boolean False) (Num 1) (Num 2)) -- Just (Num 2)

    -- Optimize Tests
    print $ optimize (Plus (Num 5) (Num 0))              -- Num 5
    print $ optimize (If (Boolean False) (Num 10) (Num 3)) -- Num 3
    print $ optimize (If (Boolean True) (Num 9) (Num 2)) -- Num 9
    print $ optimize (Plus (Num 3) (Num 4))               -- Num 7

    -- evalOptM Tests
    print $ evalOptM (Plus (Num 6) (Num 6))               -- Just (Num 12)
    print $ evalOptM (If (Boolean True) (Num 4) (Num 5)) -- Just (Num 4)
    print $ evalOptM (If (Boolean False) (Num 3) (Num 8)) -- Just (Num 8)
    print $ evalOptM (Div (Num 9) (Num 0))                -- Nothing (Division by zero)
