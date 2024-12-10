-- Project 2 EECS 662
-- Dylan A Davis
-- 3047302
-- Professor Perry Alexander
-- 11/3/2024
--



{-# LANGUAGE GADTs #-}

-- Imports for Monads

import Control.Monad

-- FAE AST and Type Definitions

data FAE where
  Num :: Int -> FAE
  Plus :: FAE -> FAE -> FAE
  Minus :: FAE -> FAE -> FAE
  Lambda :: String -> FAE -> FAE
  App :: FAE -> FAE -> FAE
  Id :: String -> FAE
  deriving (Show,Eq)

type Env = [(String,FAE)]

evalDynFAE :: Env -> FAE -> Maybe FAE
evalDynFAE _ (Num n) = Just (Num n)  -- Evaluate numbers
evalDynFAE env (Id x) = lookup x env  -- Look up the identifier in the environment
evalDynFAE env (Plus l r) = do
    Num l' <- evalDynFAE env l  -- Evaluate left operand
    Num r' <- evalDynFAE env r  -- Evaluate right operand
    return (Num (l' + r'))  -- Return the sum
evalDynFAE env (Minus l r) = do
    Num l' <- evalDynFAE env l  -- Evaluate left operand
    Num r' <- evalDynFAE env r  -- Evaluate right operand
    if r' < l' then return (Num (l' - r')) else Nothing -- Return the difference
evalDynFAE env (Lambda x body) = Just (Lambda x body)  -- Return the lambda as is
evalDynFAE env (App f arg) = do
    argV <- evalDynFAE env arg  -- Evaluate the argument first
    func <- evalDynFAE env f    -- Evaluate the function
    case func of
        Lambda x body -> evalDynFAE ((x, argV) : env) body  -- Apply the function with the argument
        _ -> Nothing  -- This case should theoretically never happen if only lambdas are used

data FAEValue where
  NumV :: Int -> FAEValue
  ClosureV :: String -> FAE -> Env' -> FAEValue
  deriving (Show,Eq)

type Env' = [(String,FAEValue)]

evalStatFAE :: Env' -> FAE -> Maybe FAEValue
evalStatFAE _ (Num n) = return (NumV n)  -- Evaluate numbers and return as NumV
evalStatFAE env (Id x) = lookup x env  -- Look up the identifier in the environment
evalStatFAE env (Plus l r) = do
    (NumV l') <- evalStatFAE env l  -- Evaluate left operand
    (NumV r') <- evalStatFAE env r  -- Evaluate right operand
    return (NumV (l' + r'))  -- Return the sum as NumV
evalStatFAE env (Minus l r) = do
    (NumV l') <- evalStatFAE env l  -- Evaluate left operand
    (NumV r') <- evalStatFAE env r  -- Evaluate right operand
    if r' < l' then return (NumV (l' - r')) else Nothing  -- Return the difference as NumV
evalStatFAE env (Lambda x body) = return (ClosureV x body env)  -- Capture the environment in the closure
evalStatFAE env (App f arg) = do
    argV <- evalStatFAE env arg  -- Evaluate the argument first
    (ClosureV x body closureEnv) <- evalStatFAE env f  -- Evaluate the function
    evalStatFAE ((x, argV) : closureEnv) body  -- Apply the closure with the argument

-- FBAE AST and Type Definitions

data FBAE where
  NumD :: Int -> FBAE
  PlusD :: FBAE -> FBAE -> FBAE
  MinusD :: FBAE -> FBAE -> FBAE
  LambdaD :: String -> FBAE -> FBAE
  AppD :: FBAE -> FBAE -> FBAE
  BindD :: String -> FBAE -> FBAE -> FBAE
  IdD :: String -> FBAE
  deriving (Show,Eq)

elabFBAE :: FBAE -> FAE
elabFBAE (NumD n) = Num n
elabFBAE (IdD x) = Id x
elabFBAE (PlusD l r) = Plus (elabFBAE l) (elabFBAE r)
elabFBAE (MinusD l r) = Minus (elabFBAE l) (elabFBAE r)
elabFBAE (BindD id t0 t1) = App (Lambda id (elabFBAE t1)) (elabFBAE t0)  -- Translate bind
elabFBAE (LambdaD id body) = Lambda id (elabFBAE body)
elabFBAE (AppD f arg) = App (elabFBAE f) (elabFBAE arg)

evalFBAE :: Env' -> FBAE -> Maybe FAEValue
evalFBAE env fbae = evalStatFAE env (elabFBAE fbae)

-- FBAEC AST and Type Definitions


-- Testing 

main :: IO ()
main = do
    putStrLn "==================\n\n\n=================="

    -- Dynamic Scoping Tests
    let env = [("x", Num 10), ("y", Num 5)]
    putStrLn "Results of evalDynFAE:"
    putStrLn $ "Id x: " ++ show (evalDynFAE env (Id "x"))  -- Should return Just (Num 10)
    putStrLn $ "Id y: " ++ show (evalDynFAE env (Id "y"))  -- Should return Just (Num 5)
    putStrLn $ "x + y: " ++ show (evalDynFAE env (Plus (Id "x") (Id "y")))  -- Should return Just (Num 15)
    putStrLn $ "Lambda application (lambda z in x + z) applied to 5: " ++
               show (evalDynFAE env (App (Lambda "z" (Plus (Id "x") (Id "z"))) (Num 5)))  -- Should return Just (Num 15)
    putStrLn $ "5 - 2: " ++ show (evalDynFAE env (Minus (Num 5) (Num 2)))  -- Should return Just (Num 3)
    putStrLn $ "Lambda application (lambda z in z - 2) applied to 5: " ++
               show (evalDynFAE env (App (Lambda "z" (Minus (Id "z") (Num 2))) (Num 5)))  -- Should return Just (Num 3)
    putStrLn "Dynamic Scoping Result:"
    putStrLn $ "Result: " ++ show (evalDynFAE env (App (Lambda "y" (Plus (Id "x") (Id "y"))) (Num 5)))  -- Should return Just (Num 15)
    putStrLn "==================\n\n\n=================="

    -- Static Scoping Tests
    let env' = [("x", NumV 10), ("y", NumV 5)]
    putStrLn "Results of evalStatFAE:"
    putStrLn $ "Id x: " ++ show (evalStatFAE env' (Id "x"))  -- Should return Just (NumV 10)
    putStrLn $ "Id y: " ++ show (evalStatFAE env' (Id "y"))  -- Should return Just (NumV 5)
    putStrLn $ "x + y: " ++ show (evalStatFAE env' (Plus (Id "x") (Id "y")))  -- Should return Just (NumV 15)
    putStrLn $ "Lambda application (lambda z in x + z) applied to 5: " ++
               show (evalStatFAE env' (App (Lambda "z" (Plus (Id "x") (Id "z"))) (Num 5)))  -- Should return Just (NumV 15)
    putStrLn $ "5 - 2: " ++ show (evalStatFAE env' (Minus (Num 5) (Num 2)))  -- Should return Just (NumV 3)
    putStrLn $ "Lambda application (lambda z in z - 2) applied to 5: " ++
               show (evalStatFAE env' (App (Lambda "z" (Minus (Id "z") (Num 2))) (Num 5)))  -- Should return Just (NumV 3)
    putStrLn "Static Scoping Result:"
    putStrLn $ "Result: " ++ show (evalStatFAE env' (App (Lambda "y" (Plus (Id "x") (Id "y"))) (Num 5)))  -- Should return Just (NumV 15)
    putStrLn "==================\n\n\n=================="