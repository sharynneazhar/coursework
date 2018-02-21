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
-- Parser and interpreter with optimizer for the ABE language  
-- for predicting failure
--
-- Author: Perry Alexander
-- Date: Feb 8, 2018
--
-- Modified by: Sharynne Azhar
-- Date: Feb 15, 2018
-- KUID: 2513206
--

-- AST Definition

data TABE where
  TNum :: TABE
  TBool :: TABE
  deriving (Show,Eq)

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
  deriving (Show,Eq)

-- AST Pretty Printer

pprint :: ABE -> String
pprint (Num n) = show n
pprint (Boolean b) = show b
pprint (Plus n m) = "(" ++ pprint n ++ " + " ++ pprint m ++ ")"
pprint (Minus n m) = "(" ++ pprint n ++ " - " ++ pprint m ++ ")"
pprint (Mult n m) = "(" ++ pprint n ++ " * " ++ pprint m ++ ")"
pprint (Div n m) = "(" ++ pprint n ++ " / " ++ pprint m ++ ")"
pprint (And n m) = "(" ++ pprint n ++ " && " ++ pprint m ++ ")"
pprint (Leq n m) = "(" ++ pprint n ++ " <= " ++ pprint m ++ ")"
pprint (IsZero m) = "(isZero " ++ pprint m ++ ")"
pprint (If c n m) = "(if " ++ pprint c ++ " then " ++ pprint n ++ " else " ++ pprint m ++ ")"

-- Parser (Requires ParserUtils and Parsec)

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

expr :: Parser ABE
expr = buildExpressionParser opTable term

opTable = [ [ inFix "*" Plus AssocLeft
            , inFix "/" Minus AssocLeft ]
          , [ inFix "+" Plus AssocLeft
            , inFix "-" Minus AssocLeft ]
          , [ inFix "<=" Leq AssocLeft
            , preFix "isZero" IsZero ]
          , [ inFix "&&" And AssocLeft ]
          ]

numExpr :: Parser ABE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

trueExpr :: Parser ABE
trueExpr = do i <- reserved lexer "true"
              return (Boolean True)

falseExpr :: Parser ABE
falseExpr = do i <- reserved lexer "false"
               return (Boolean False)

ifExpr :: Parser ABE
ifExpr = do reserved lexer "if"
            c <- expr
            reserved lexer "then"
            t <- expr
            reserved lexer "else"
            e <- expr
            return (If c t e)

term = parens lexer expr
       <|> numExpr
       <|> trueExpr
       <|> falseExpr
       <|> ifExpr

-- Parser invocation

parseABE = parseString expr

-- Helper lift Functions

liftNum :: (Int -> Int -> Int) -> ABE -> ABE -> ABE
liftNum f (Num x) (Num y) = Num (f x y)

liftNum2Bool :: (Int -> Int -> Bool) -> ABE -> ABE -> ABE
liftNum2Bool f (Num x) (Num y) = Boolean (f x y)

liftBool :: (Bool -> Bool -> Bool) -> ABE -> ABE -> ABE
liftBool f (Boolean x) (Boolean y) = Boolean (f x y)

-- Evaluation Functions

evalM :: ABE -> (Maybe ABE)
evalM (Num n) = return (Num n)
evalM (Boolean b) = return (Boolean b)

evalM (Plus l r) = 
  do x <- evalM l;
     y <- evalM r;
     return (liftNum (+) x y)

evalM (Minus l r) = 
  do x <- evalM l;
     y <- evalM r;
     return (liftNum (-) x y)

evalM (Mult l r) =
  do x <- evalM l;
     y <- evalM r;
     return (liftNum (*) x y)

evalM (Div l r) =
  do x <- evalM l;
     y <- evalM r;
     return (liftNum div x y)

evalM (And l r) =
  do x <- evalM l;
     y <- evalM r;
     return (liftBool (&&) x y)

evalM (Leq l r) =
  do x <- evalM l;
     y <- evalM r;
     return (liftNum2Bool (<=) x y)

evalM (IsZero t) =
  do x <- evalM t;
     return (liftNum2Bool (==) x (Num 0))

evalM (If c t e) =
  do Boolean b <- evalM c;
     if b then (evalM t) 
     else (evalM e)

evalErr :: ABE -> (Maybe ABE)
evalErr (Num n) = return (Num n)
evalErr (Boolean b) = return (Boolean b)  

evalErr (Plus l r) =
  do x <- evalErr l;
     y <- evalErr r;
     case x of 
       Num l2 -> case y of
         Num r2 -> return (Num (l2 + r2))
         _ -> Nothing
       _ -> Nothing

evalErr (Minus l r) =
  do x <- evalErr l;
     y <- evalErr r;
     case x of 
       Num l2 -> case y of
         Num r2 -> return (Num (l2 - r2))
         _ -> Nothing
       _ -> Nothing

evalErr (Mult l r) =
  do x <- evalErr l;
     y <- evalErr r;
     case x of 
       Num l2 -> case y of
         Num r2 -> return (Num (l2 * r2))
         _ -> Nothing
       _ -> Nothing

evalErr (Div l r) =
  do x <- evalErr l;
     y <- evalErr r;
     case x of 
       Num l2 -> case y of
         Num r2 -> if r2 == 0 then Nothing
                   else Just (Num (div l2 r2))
         _ -> Nothing
       _ -> Nothing

evalErr (And l r) =
  do x <- evalErr l;
     y <- evalErr r;
     case x of 
       Boolean l2 -> case y of
         Boolean r2 -> return (Boolean (l2 && r2))
         _ -> Nothing
       _ -> Nothing

evalErr (Leq l r) =
  do x <- evalErr l;
     y <- evalErr r;
     case x of 
       Boolean l2 -> case y of
         Boolean r2 -> return (Boolean (l2 <= r2))
         _ -> Nothing
       _ -> Nothing

evalErr (IsZero t) =
  do r <- evalErr t;
     case r of
       Num v -> return (Boolean (v == 0))
       _ -> Nothing 

evalErr (If c t e) =
  do r <- evalErr c;
     case r of
      Boolean v -> if v then (evalErr t) else (evalErr e)
      _ -> Nothing

-- Type Derivation Function

typeofM :: ABE -> Maybe TABE
typeofM (Num n) = return TNum
typeofM (Boolean b) = return TBool

typeofM (Plus l r) = 
  do x <- typeofM l;
     y <- typeofM r;
     if x == TNum && y == TNum then return TNum
     else Nothing

typeofM (Minus l r) = 
  do x <- typeofM l;
     y <- typeofM r;
     if x == TNum && y == TNum then return TNum
     else Nothing

typeofM (Mult l r) =
  do x <- typeofM l;
     y <- typeofM r;
     if x == TNum && y == TNum then return TNum
     else Nothing

typeofM (Div l r) =
  do x <- typeofM l;
     y <- typeofM r;
     if x == TNum && y == TNum then return TNum
     else Nothing

typeofM (And l r) =
  do x <- typeofM l;
     y <- typeofM r;
     if x == TBool && y == TBool then return TBool
     else Nothing
  
typeofM (Leq l r) =
  do x <- typeofM l;
     y <- typeofM r;
     if x == TNum && y == TNum then return TBool
     else Nothing

typeofM (IsZero t) =
  do x <- typeofM t;
     if x == TNum then return TBool
     else Nothing

typeofM (If c t e) =
  do c2 <- typeofM c;
     t2 <- typeofM t;
     e2 <- typeofM e;
     if c2 == TBool && t2 == e2 then return t2
     else Nothing

-- Combined interpreter

evalTypeM :: ABE -> Maybe ABE
evalTypeM e = 
  do typeofM e;
     evalM e;

-- Optimizer

optimize :: ABE -> ABE
optimize (Plus l (Num 0)) = l
optimize (Plus (Num 0) r) = r
optimize (If (Boolean True) l r) = l
optimize (If (Boolean False) l r) = r
optimize e = e

interpOptM :: ABE -> Maybe ABE
interpOptM = evalM . optimize

-- Testing (Requires QuickCheck 2)

-- Arbitrary AST Generator

instance Arbitrary ABE where
  arbitrary =
    sized $ \n -> genABE (rem n 10)

genNum =
  do t <- choose (0,100)
     return (Num t)

genBool =
  do t <- choose (True,False)
     return (Boolean t)

genPlus n =
  do s <- genABE n
     t <- genABE n
     return (Plus s t)

genMinus n =
  do s <- genABE n
     t <- genABE n
     return (Minus s t)

genAnd n =
  do s <- genABE n
     t <- genABE n
     return (And s t)

genLeq n =
  do s <- genABE n
     t <- genABE n
     return (Leq s t)

genIsZero n =
  do s <- genABE n
     return (IsZero s)

genIf n =
  do s <- genABE n
     t <- genABE n
     u <- genABE n
     return (If s t u)

genABE :: Int -> Gen ABE
genABE 0 = 
  do term <- oneof [genNum,genBool]
     return term
genABE n =
  do term <- oneof [genNum,(genPlus (n-1))
                   ,(genMinus (n-1))
                   ,(genAnd (n-1))
                   ,(genLeq (n-1))
                   ,(genIsZero (n-1))
                   ,(genIf (n-1))]
     return term

-- QuickCheck 

testParser :: Int -> IO ()
testParser n = verboseCheckWith stdArgs {maxSuccess=n}
  (\t -> parseABE (pprint t) == t)

testTypeof :: Int -> IO ()
testTypeof n = verboseCheckWith stdArgs {maxSuccess=n}
  (\t -> case typeofM t of
           Just _ -> True
           Nothing -> True)

testTypedEval :: Int -> IO ()
testTypedEval n = verboseCheckWith stdArgs {maxSuccess=n}
  (\t -> case typeofM t of
           Just _ -> interpOptM (parseABE (pprint t)) == (evalM t)
           Nothing -> True)