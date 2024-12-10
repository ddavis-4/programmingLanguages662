
-- Project 3 EECS 662
-- Dylan A Davis
-- 3047302
-- Professor Perry Alexander
-- 11/22/2024
--

{-# LANGUAGE GADTs #-}

-- import Control.Monad

-- Calculator language extended with an environment to hold defined variables

data TFBAE where
  TNum :: TFBAE
  TBool :: TFBAE
  (:->:) :: TFBAE -> TFBAE -> TFBAE
  deriving (Show,Eq)

data FBAE where
  Num :: Int -> FBAE
  Plus :: FBAE -> FBAE -> FBAE
  Minus :: FBAE -> FBAE -> FBAE
  Mult :: FBAE -> FBAE -> FBAE
  Div :: FBAE -> FBAE -> FBAE
  Bind :: String -> FBAE -> FBAE -> FBAE
  Lambda :: String -> TFBAE -> FBAE -> FBAE
  App :: FBAE -> FBAE -> FBAE
  Id :: String -> FBAE
  Boolean :: Bool -> FBAE
  And :: FBAE -> FBAE -> FBAE
  Or :: FBAE -> FBAE -> FBAE
  Leq :: FBAE -> FBAE -> FBAE
  IsZero :: FBAE -> FBAE
  If :: FBAE -> FBAE -> FBAE -> FBAE
  Fix :: FBAE -> FBAE
  deriving (Show,Eq)

-- Value defintion for statically scoped eval

data FBAEVal where
  NumV :: Int -> FBAEVal
  BooleanV :: Bool -> FBAEVal
  ClosureV :: String -> FBAE -> Env -> FBAEVal
  deriving (Show,Eq)

-- Substitution

subst :: String -> FBAE -> FBAE -> FBAE
subst _ _ (Num x) = (Num x)
subst i v (Plus l r) = (Plus (subst i v l) (subst i v r))
subst i v (Minus l r) = (Minus (subst i v l) (subst i v r))
subst i v (Mult l r) = (Mult (subst i v l) (subst i v r))
subst i v (Div l r) = (Div (subst i v l) (subst i v r))
subst i v (Bind i' v' b') = if i==i'
                            then (Bind i' (subst i v v') b')
                            else (Bind i' (subst i v v') (subst i v b'))
subst i v (Lambda i' ty b') = if i==i'
                             then (Lambda i' ty b')
                             else (Lambda i' ty (subst i v b'))
subst i v (App f a) = (App (subst i v f) (subst i v a))
subst i v (Id i') = if i==i'
                    then v
                    else (Id i')
subst i v (Boolean b) = (Boolean b)
subst i v (And l r) = (And (subst i v l) (subst i v r))
subst i v (Or l r) = (Or (subst i v l) (subst i v r))
subst i v (Leq l r) = (Leq (subst i v l) (subst i v r))
subst i v (IsZero t) = (IsZero (subst i v t))
subst i v (If c l r) = (If (subst i v c) (subst i v l) (subst i v r))
subst i v (Fix t) = (Fix (subst i v t))




-- Enviornment for statically scoped eval

type Env = [(String,FBAEVal)]

-- Statically scoped eval
         
evalM :: Env -> FBAE -> (Maybe FBAEVal)
evalM env (Num n) = return (NumV n)

evalM env (Boolean b) = return (BooleanV b)

evalM env (Lambda i _ b) = return (ClosureV i b env)

evalM env (Plus l r) = do
    NumV l' <- evalM env l
    NumV r' <- evalM env r
    return (NumV (l' + r'))

evalM env (Minus l r) = do
    NumV l' <- evalM env l
    NumV r' <- evalM env r
    if l' < r' then Nothing else return (NumV (l' - r'))

evalM env (Mult l r) = do
    NumV l' <- evalM env l
    NumV r' <- evalM env r
    return (NumV (l' * r'))

evalM env (Div l r) = do
    NumV l' <- evalM env l
    NumV r' <- evalM env r
    if r' == 0 then Nothing else return (NumV (l' `div` r'))

evalM env (And l r) = do
    BooleanV l' <- evalM env l
    BooleanV r' <- evalM env r
    return (BooleanV (l' && r'))

evalM env (Or l r) = do
    BooleanV l' <- evalM env l
    BooleanV r' <- evalM env r
    return (BooleanV (l' || r'))

evalM env (Leq l r) = do
    NumV l' <- evalM env l
    NumV r' <- evalM env r
    return (BooleanV (l' <= r'))

evalM env (IsZero n) = do
    NumV n' <- evalM env n
    return (BooleanV (n' == 0))

evalM env (Id x) = lookup x env

evalM env (Bind i v b) = do
    vVal <- evalM env v
    evalM ((i, vVal) : env) b

evalM env (App f a) = do
    ClosureV i b clo <- evalM env f
    aVal <- evalM env a
    evalM ((i, aVal) : clo) b

evalM env (If c t e) = do
    BooleanV cond <- evalM env c
    if cond then evalM env t else evalM env e

evalM env (Fix f) = do
    ClosureV g b clo <- evalM env f
    evalM clo (subst g (Fix (Lambda g TNum b)) b)


-- Type inference function

type Cont = [(String,TFBAE)]

typeofM :: Cont -> FBAE -> (Maybe TFBAE)
typeofM cont (Num _) = return TNum

typeofM cont (Boolean _) = return TBool

typeofM cont (Plus l r) = do
    TNum <- typeofM cont l
    TNum <- typeofM cont r
    return TNum

typeofM cont (Minus l r) = do
    TNum <- typeofM cont l
    TNum <- typeofM cont r
    return TNum

typeofM cont (Mult l r) = do
    TNum <- typeofM cont l
    TNum <- typeofM cont r
    return TNum

typeofM cont (Div l r) = do
    TNum <- typeofM cont l
    TNum <- typeofM cont r
    return TNum

typeofM cont (And l r) = do
    TBool <- typeofM cont l
    TBool <- typeofM cont r
    return TBool

typeofM cont (Or l r) = do
    TBool <- typeofM cont l
    TBool <- typeofM cont r
    return TBool

typeofM cont (Leq l r) = do
    TNum <- typeofM cont l
    TNum <- typeofM cont r
    return TBool

typeofM cont (IsZero n) = do
    TNum <- typeofM cont n
    return TBool

typeofM cont (If c t e) = do
    TBool <- typeofM cont c
    tType <- typeofM cont t
    eType <- typeofM cont e
    if tType == eType then return tType else Nothing

typeofM cont (Id i) = lookup i cont

typeofM cont (Bind i v b) = do
    vType <- typeofM cont v
    typeofM ((i, vType) : cont) b

typeofM cont (Lambda x ty body) = do
    bodyType <- typeofM ((x, ty) : cont) body
    return (ty :->: bodyType)

typeofM cont (App f a) = do
    (argType :->: retType) <- typeofM cont f
    aType <- typeofM cont a
    if aType == argType then return retType else Nothing

typeofM cont (Fix f) = do
    (argType :->: retType) <- typeofM cont f
    if argType == retType then return retType else Nothing


-- Interpreter

interp :: FBAE -> (Maybe FBAEVal)
interp exp = do
  typeofM [] exp;
  evalM [] exp

-- Factorial function for testing evalM and typeofM.  the type of test1 should
-- be TNum and the result of evaluating test1`should be (NumV 6).  Remember
-- that Just is used to return both the value and type.

-- Test cases

test1 = (Bind "f" (Lambda "g" ((:->:) TNum TNum)
                    (Lambda "x" TNum (If (IsZero (Id "x")) (Num 1)
                                         (Mult (Id "x")
                                               (App (Id "g")
                                                    (Minus (Id "x")
                                                           (Num 1)))))))
         (App (Fix (Id "f")) (Num 3)))

-- Test case for a simple addition
test2 = Plus (Num 5) (Num 3)  -- Should evaluate to NumV 8

-- Test case for a simple subtraction
test3 = Minus (Num 10) (Num 4)  -- Should evaluate to NumV 6

-- Test case for a simple multiplication
test4 = Mult (Num 7) (Num 6)  -- Should evaluate to NumV 42

-- Test case for a simple division
test5 = Div (Num 20) (Num 4)  -- Should evaluate to NumV 5

-- Test case for division by zero
test6 = Div (Num 10) (Num 0)  -- Should evaluate to Nothing

-- Test case for a logical AND operation
test7 = And (Boolean True) (Boolean True)  -- Should evaluate to BooleanV False

-- Test case for a logical OR operation
test8 = Or (Boolean False) (Boolean True)  -- Should evaluate to BooleanV True

-- Test case for a less than or equal operation
test9 = Leq (Num 5) (Num 10)  -- Should evaluate to BooleanV True

-- Test case for a zero check
test10 = IsZero (Num 0)  -- Should evaluate to BooleanV True

-- Test case for a factorial function with a larger number
test11 = (Bind "f" (Lambda "g" ((:->:) TNum TNum)
                    (Lambda "x" TNum (If (IsZero (Id "x")) (Num 1)
                                         (Mult (Id "x")
                                               (App (Id "g")
                                                    (Minus (Id "x")
                                                           (Num 1)))))))
         (App (Fix (Id "f")) (Num 5)))  -- Should evaluate to NumV 120

-- Test case for nested function calls
test12 = (Bind "double" (Lambda "x" TNum (Mult (Id "x") (Num 2)))
         (App (Id "double") (Num 10)))  -- Should evaluate to NumV 20

-- Test case for using a variable in a binding
test13 = (Bind "x" (Num 3)
          (Bind "y" (Plus (Id "x") (Num 2))
             (Plus (Id "x") (Id "y"))))  -- Should evaluate to NumV 8


-- Test case for a less than or equal operation
test14 = Leq (Num 10) (Num 5)  -- Should evaluate to BooleanV False


-- Main function 


main :: IO ()
main = do
  putStrLn "Running Test 1: Factorial of 3"
  print $ typeofM [] test1 -- Just TNum
  print $ evalM [] test1   -- Just (NumV 6)

  putStrLn "\nRunning Test 2: Simple Addition (5 + 3)"
  print $ typeofM [] test2  -- Just TNum
  print $ evalM [] test2     -- Just (NumV 8)

  putStrLn "\nRunning Test 3: Simple Subtraction (10 - 4)"
  print $ typeofM [] test3  -- Just TNum
  print $ evalM [] test3     -- Just (NumV 6)

  putStrLn "\nRunning Test 4: Simple Multiplication (7 * 6)"
  print $ typeofM [] test4  -- Just TNum
  print $ evalM [] test4     -- Just (NumV 42)

  putStrLn "\nRunning Test 5: Simple Division (20 / 4)"
  print $ typeofM [] test5  -- Just TNum
  print $ evalM [] test5     -- Just (NumV 5)

  putStrLn "\nRunning Test 6: Division by Zero (10 / 0)"
  print $ typeofM [] test6  -- Just TNum
  print $ evalM [] test6     -- Nothing

  putStrLn "\nRunning Test 7: Logical AND (True && False)"
  print $ typeofM [] test7  -- Just TBool
  print $ evalM [] test7     -- Just (BooleanV False)

  putStrLn "\nRunning Test 8: Logical OR (False || True)"
  print $ typeofM [] test8  -- Just TBool
  print $ evalM [] test8     -- Just (BooleanV True)

  putStrLn "\nRunning Test 9: Less than or equal (5 <= 10)"
  print $ typeofM [] test9  -- Just TBool
  print $ evalM [] test9     -- Just (BooleanV True)

  putStrLn "\nRunning Test 10: Zero Check (IsZero 0)"
  print $ typeofM [] test10  -- Just TBool
  print $ evalM [] test10    -- Just (BooleanV True)

  putStrLn "\nRunning Test 11: Factorial of 5"
  print $ typeofM [] test11  -- Just TNum
  print $ evalM [] test11    -- Just (NumV 120)

  putStrLn "\nRunning Test 12: Double of 10"
  print $ typeofM [] test12  -- Just TNum
  print $ evalM [] test12    -- Just (NumV 20)

  putStrLn "\nRunning Test 13: Using variables (x = 3, y = x + 2, x + y = 8)"
  print $ typeofM [] test13  -- Just TNum
  print $ evalM [] test13    -- Just (NumV 8)

  putStrLn "\nRunning Test 14: Less than or equal (10 <= 5)"
  print $ typeofM [] test14  -- Just TBool
  print $ evalM [] test14     -- Just (BooleanV False)







