{-# LANGUAGE GADTs, FlexibleContexts #-}

-- Imports for QuickCheck
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Function
import Test.QuickCheck.Monadic

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
-- Modified by: Sharynne Azhar
-- Date: Tue Feb 6 15:08:00 CST 2018
--
-- Source files for the Arithmetic Expressions (AE) language from PLIH
--

-- AST Definition
data AE where
  Num :: Int -> AE
  Plus :: AE -> AE -> AE
  Minus :: AE -> AE -> AE
  Mult :: AE -> AE -> AE
  Div :: AE -> AE -> AE
  If0 :: AE -> AE -> AE -> AE
  deriving (Show,Eq)

-- AST Pretty Printer
-- Prints concrete syntax from abstract syntax
-- Ref: ae.hs from http://ku-sldg.github.io/plih//simple/1-Arithmetic-Expressions.html
pprintAE :: AE -> String
pprintAE (Num n) = show n
pprintAE (Plus n m) = "(" ++ pprintAE n ++ "+" ++ pprintAE m ++ ")"
pprintAE (Minus n m) = "(" ++ pprintAE n ++ "-" ++ pprintAE m ++ ")"
pprintAE (Mult n m) = "(" ++ pprintAE n ++ "*" ++ pprintAE m ++ ")"
pprintAE (Div n m) = "(" ++ pprintAE n ++ "/" ++ pprintAE m ++ ")"
pprintAE (If0 c t e) = "(if0 " ++ pprintAE c ++ " then " ++ pprintAE t ++ " else " ++ pprintAE e ++ ")"

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

inFix o c a = (Infix (reservedOp lexer o >> return c) a)
preFix o c = (Prefix (reservedOp lexer o >> return c))
postFix o c = (Postfix (reservedOp lexer o >> return c))

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
             return (Num (fromInteger i))

ifExpr :: Parser AE
ifExpr  = do reserved lexer "if0"
             c <- expr
             reserved lexer "then"
             t <- expr
             reserved lexer "else"
             e <- expr
             return (If0 c t e)


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

evalAE :: AE -> Int
evalAE (Num a) = a
evalAE (Plus l r) = (evalAE l) + (evalAE r)
evalAE (Minus l r) = if ((evalAE l) - (evalAE r) < 0) then error "!"
                     else (evalAE l) - (evalAE r)
evalAE (Mult l r) = (evalAE l) * (evalAE r)
evalAE (Div l r) = if ((evalAE r) == 0) then error "!"
                   else div (evalAE l) (evalAE r)
evalAE (If0 c t e) = if ((evalAE c) == 0) then (evalAE t)
                     else (evalAE e)

evalAEMaybe :: AE -> Maybe Int
evalAEMaybe (Num a) = Just a
evalAEMaybe (Plus l r) =
  case (evalAEMaybe l) of
    Just l2 -> case (evalAEMaybe r) of
      Just r2 -> Just (l2 + r2)
      Nothing -> Nothing
    Nothing -> Nothing

evalAEMaybe (Minus l r) =
  case (evalAEMaybe l) of
    Just l2 -> case (evalAEMaybe r) of
      Just r2 -> if (l2 - r2 < 0) then Nothing
        else Just (l2 - r2)
      Nothing -> Nothing
    Nothing -> Nothing

evalAEMaybe (Mult l r) =
  case (evalAEMaybe l) of
    Just l2 -> case (evalAEMaybe r) of
      Just r2 -> Just (l2 * r2)
      Nothing -> Nothing
    Nothing -> Nothing

evalAEMaybe (Div l r) =
  case (evalAEMaybe l) of
    Just l2 -> case (evalAEMaybe r) of
      Just r2 -> if (r2 == 0) then Nothing
                 else Just (div l2 r2)
      Nothing -> Nothing
    Nothing -> Nothing

evalAEMaybe (If0 c t e) =
  case (evalAEMaybe c) of
    Just c2 -> if (c2 == 0) then (evalAEMaybe t)
               else (evalAEMaybe e)
    Nothing -> Nothing

evalM :: AE -> Maybe Int
evalM (Num a) = return a
evalM (Plus l r) =
  do x <- evalM l;
     y <- evalM r;
     return (x + y)

evalM (Minus l r) =
  do x <- evalM l;
     y <- evalM r;
     if (x - y < 0) then Nothing
     else Just (x - y)

evalM (Mult l r) =
  do x <- evalM l;
     y <- evalM r;
     return (x * y)

evalM (Div l r) =
  do x <- evalM l;
     y <- evalM r;
     if (y == 0) then Nothing
     else Just (div x y)

evalM (If0 c t e) =
  do x <- evalM c;
     if (x == 0) then (evalM t)
     else (evalM e)

interpAE :: String -> Maybe Int
interpAE = evalM . parseAE

-- QuickCheck Testing
-- Reference: http://ku-sldg.github.io/plih//simple/2-Testing-AE.html

instance Arbitrary AE where
  arbitrary =
    sized $ \n -> genAE ((rem n 5) + 5)

genNum =
  do t <- choose (0,100)
     return (Num t)

genPlus n =
  do s <- genAE n
     t <- genAE n
     return (Plus s t)

genMinus n =
  do s <- genAE n
     t <- genAE n
     return (Minus s t)

genMult n =
  do s <- genAE n
     t <- genAE n
     return (Minus s t)

genDiv n =
  do s <- genAE n
     t <- genAE n
     return (Mult s t)

genIf n =
  do c <- genAE n
     t <- genAE n
     e <- genAE n
     return (If0 c t e)

genAE :: Int -> Gen AE
genAE 0 =
  do term <- genNum
     return term
genAE n =
  do term <- oneof [genNum
                   , (genPlus (n-1))
                   , (genMinus (n-1))
                   , (genMult (n-1))
                   , (genDiv (n-1))
                   , (genIf (n-1))]
     return term


-- QuickCheck Tests
testParser :: Int -> IO ()
testParser n = verboseCheckWith stdArgs {maxSuccess=n}
  (\t -> parseAE (pprintAE t) == t)

testEval :: Int -> IO ()
testEval n = verboseCheckWith stdArgs {maxSuccess=n}
  (\t -> (interpAE $ pprintAE t) == (evalM t))
