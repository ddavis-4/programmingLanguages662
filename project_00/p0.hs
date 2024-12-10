{-# LANGUAGE GADTs, FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}

-- Imports for Parsec
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

--
-- Simple caculator over naturals with no identifiers
--
-- Author: Perry Alexander
-- Date: Tue Jan 23 17:54:44 CST 2018
--
-- Project 0 EECS 662
-- Dylan A Davis
-- 3047302
-- 9/12/2024
--
--
--
-- Source files for the Arithmetic Expressions (AE) language from PLIH
--




-- AST Definition

data AE where
  Nat :: Int -> AE
  Plus :: AE -> AE -> AE
  Minus :: AE -> AE -> AE
  Mult :: AE -> AE -> AE
  Div :: AE -> AE -> AE
  If0 :: AE -> AE -> AE -> AE
  deriving (Show,Eq)

-- AE Parser (Requires ParserUtils and Parsec included above)

languageDef =
  javaStyle { identStart = letter
            , identLetter = alphaNum
            , reservedNames = [ "if0"
                              , "then"
                              , "else"
                              ]
            , reservedOpNames = [ "+","-","*","/"]
            }

lexer = makeTokenParser languageDef

inFix o c = Infix (reservedOp lexer o >> return c)
preFix o c = Prefix (reservedOp lexer o >> return c)
postFix o c = Postfix (reservedOp lexer o >> return c)

parseString p str =
  case parse p "" str of
    Left e -> error $ show e
    Right r -> r

expr :: Parser AE
expr = buildExpressionParser operators term

operators = [
  [ inFix "*" Mult AssocLeft
    , inFix "/" Div AssocLeft ]
  , [ inFix "+" Plus AssocLeft
  , inFix "-" Minus AssocLeft ]
  ]

numExpr :: Parser AE
numExpr = do i <- integer lexer
             return (Nat (fromInteger i))

ifExpr :: Parser AE
ifExpr  = do reserved lexer "if0"
             c <- expr
             reserved lexer "then"
             t <- expr
             reserved lexer "else"
             If0 c t <$> expr


term = parens lexer expr
       <|> numExpr
       <|> ifExpr

-- Parser invocation
-- Call parseAE to parse a string into the AE data structure.

parseAE = parseString expr

-- Evaluation Functions
-- Replace the bodies of these functions with your implementations for
-- Exercises 1-4.  Feel free to add utility functions or testing functions as
-- you see fit, but do not change the function signatures.  Note that only
-- Exercise 4 requires you to integrate the parser above.

-- Test
-- evalAE (Plus l r) = if evalAE l < 0 || evalAE r < 
-- then error "All Numbers Must be Natural"
-- else evalAE l + evalAE r

-- The new construct, if0, is an expression that evaluates its 
-- first argument and if it is 0 evaluates its second. 
-- If not it evaluates its third.

-- Exercise 1 --

evalAE :: AE -> Int

evalAE (Nat n) = if n < 0 then error "All Numbers Must be Natural" else n

evalAE (Plus l r) = evalAE l + evalAE r

evalAE (Minus l r)
  | evalAE l < evalAE r = error "This difference is Negative"
  | otherwise = evalAE l - evalAE r

evalAE (Mult l r) = evalAE l * evalAE r

evalAE (Div l r)
  | evalAE r == 0 = error "You cannot divide by 0"
  | otherwise = evalAE l `div` evalAE r

evalAE (If0 a b c)
  | evalAE a < 0 || evalAE b < 0 || evalAE c < 0 = error "All Numbers Must be Naturals"
  | evalAE a == 0 = evalAE b
  | otherwise = evalAE c


-- Exercise 2 --


evalAEMaybe :: AE -> Maybe Int

evalAEMaybe (Nat n) = if n < 0 then Nothing else Just n

evalAEMaybe (Plus l r) = case evalAEMaybe l of
    Nothing -> Nothing
    Just l' -> case evalAEMaybe r of
        Nothing -> Nothing
        Just r' -> Just (l' + r') 

evalAEMaybe (Minus l r) = case evalAEMaybe l of
    Nothing -> Nothing
    Just l' -> case evalAEMaybe r of
        Nothing -> Nothing
        Just r' -> if l' >= r' then Just (l' - r') else Nothing

evalAEMaybe (Mult l r) = case evalAEMaybe l of
    Nothing -> Nothing
    Just l' -> case evalAEMaybe r of
        Nothing -> Nothing
        Just r' -> Just (l' * r')

evalAEMaybe (Div l r) = case evalAEMaybe l of
    Nothing -> Nothing
    Just l' -> case evalAEMaybe r of
        Nothing -> Nothing
        Just r' -> if r' == 0 then Nothing else Just (l' `div` r')

evalAEMaybe (If0 a b c) = case evalAEMaybe a of
    Nothing -> Nothing
    Just a' -> if a' == 0  then evalAEMaybe b else evalAEMaybe c


-- Exercise 3 --


evalM :: AE -> Maybe Int

evalM (Nat n) = if n < 0 then Nothing else Just n

evalM (Plus l r) = do
    x <- evalM l
    y <- evalM r
    Just (x + y)

evalM (Minus l r) = do
    x <- evalM l
    y <- evalM r
    if x < y then Nothing else Just (x - y)

evalM (Mult l r) = do
    x <- evalM l
    y <- evalM r
    Just (x * y)

evalM (Div l r) = do
    x <- evalM l
    y <- evalM r
    if y == 0 then Nothing else return (x `div` y)

evalM (If0 a b c) = do
    x <- evalM a
    if x == 0 then evalM b else evalM c


-- Exercise 4 -- 

interpAE :: String -> Maybe Int
interpAE x = evalM (parseAE x)


-- Tests for the Exercises --


main :: IO ()
main = do
      putStrLn "Results of evalAE:"
      putStrLn $ "Nat 3: " ++ show (evalAE (Nat 3))
      putStrLn $ "5 + 2: " ++ show (evalAE (Plus (Nat 5) (Nat 2)))
      putStrLn $ "7 - 5: " ++ show (evalAE (Minus (Nat 7) (Nat 5)))
      putStrLn $ "8 * 2: " ++ show (evalAE (Mult (Nat 8) (Nat 2)))
      putStrLn $ "4 / 4: " ++ show (evalAE (Div (Nat 4) (Nat 4)))
      putStrLn $ "If0 a = 0 then b = 5 else c = 2: " ++ show (evalAE (If0 (Nat 0) (Nat 5) (Nat 2)))
    
      putStrLn "\nResults of evalAE: Bad inputs (uncomment in code to test)"
      --putStrLn $ "Nat -3: " ++ show (evalAE (Nat (-3)))
      putStrLn $ "5 + 2: " ++ show (evalAE (Plus (Nat 5) (Nat 2)))
      --putStrLn $ "7 - 9: " ++ show (evalAE (Minus (Nat 7) (Nat 9)))
      putStrLn $ "8 * 2: " ++ show (evalAE (Mult (Nat 8) (Nat 2)))
     -- putStrLn $ "4 / 0: " ++ show (evalAE (Div (Nat 4) (Nat 0)))
      putStrLn $ "If0 a = 1 then b = 5 else c = 2: " ++ show (evalAE (If0 (Nat 1) (Nat 5) (Nat 2)))



      putStrLn "\nResults of evalAEMaybe:"
      putStrLn $ "Nat 9: " ++ show (evalAEMaybe (Nat 9))
      putStrLn $ "5 + 6: " ++ show (evalAEMaybe (Plus (Nat 5) (Nat 6)))
      putStrLn $ "14 - 13: " ++ show (evalAEMaybe (Minus (Nat 14) (Nat 13)))
      putStrLn $ "9 * 8: " ++ show (evalAEMaybe (Mult (Nat 9) (Nat 8)))
      putStrLn $ "9 / 3: " ++ show (evalAEMaybe (Div (Nat 9) (Nat 3)))
      putStrLn $ "If0 a = 0 then b = 5 else c = 4: " ++ show (evalAEMaybe (If0 (Nat 0) (Nat 5) (Nat 4)))

      putStrLn "\nResults of evalAEMaybe: Bad Inputs"
      putStrLn $ "Nat -9: " ++ show (evalAEMaybe (Nat (-9)))
      putStrLn $ "5 + 6: " ++ show (evalAEMaybe (Plus (Nat 5) (Nat 6)))
      putStrLn $ "14 - 20: " ++ show (evalAEMaybe (Minus (Nat 14) (Nat 20)))
      putStrLn $ "9 * 8: " ++ show (evalAEMaybe (Mult (Nat 9) (Nat 8)))
      putStrLn $ "9 / 0: " ++ show (evalAEMaybe (Div (Nat 9) (Nat 0)))
      putStrLn $ "If0 a = 5 then b = 5 else c = 4: " ++ show (evalAEMaybe (If0 (Nat 5) (Nat 5) (Nat 4)))     


      putStrLn "\nResults of evalM:"
      putStrLn $ "Nat 9: " ++ show (evalM (Nat 9))
      putStrLn $ "5 + 6: " ++ show (evalM (Plus (Nat 5) (Nat 6)))
      putStrLn $ "5 - 2: " ++ show (evalM (Minus (Nat 5) (Nat 2)))
      putStrLn $ "5 * 2: " ++ show (evalM (Mult (Nat 5) (Nat 2)))
      putStrLn $ "8 / 7: " ++ show (evalM (Div (Nat 8) (Nat 7)))
      putStrLn $ "If0 a = 0 then b = 6 else c = 7: " ++ show (evalM (If0 (Nat 0) (Nat 6) (Nat 7)))

      putStrLn "\nResults of evalM: Bad Inputs"
      putStrLn $ "Nat -9: " ++ show (evalM (Nat  (-9)))
      putStrLn $ "5 + 6: " ++ show (evalM (Plus (Nat 5) (Nat 6)))
      putStrLn $ "5 - 10: " ++ show (evalM (Minus (Nat 5) (Nat 10)))
      putStrLn $ "5 * 2: " ++ show (evalM (Mult (Nat 5) (Nat 2)))
      putStrLn $ "8 / 0: " ++ show (evalM (Div (Nat 8) (Nat 0)))
      putStrLn $ "If0 a = 1 then b = 6 else c = 7: " ++ show (evalM (If0 (Nat 1) (Nat 6) (Nat 7)))

      putStrLn "\nResults of InterpAE Plus"
      print (interpAE "1+1") <* print (interpAE "1+3") <* print (interpAE "1+4")
      putStrLn "\nResults of InterpAE Minus"
      print (interpAE "4-1") <* print (interpAE "9-1") <* print (interpAE "5-4") <* print (interpAE "5-6")
      putStrLn "\nResults of InterpAE Mult"
      print (interpAE "4*1") <* print (interpAE "9*9") <* print (interpAE "5*4")
      putStrLn "\nResults of InterpAE Div"
      print (interpAE "4/1") <* print (interpAE "9/9") <* print (interpAE "8/4") <* print (interpAE "8/0")