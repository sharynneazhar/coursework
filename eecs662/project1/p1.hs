{-# LANGUAGE GADTs, FlexibleContexts #-}

-- Imports for Parsec
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

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

evalM (Boolean b) = Just (Boolean b)

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
  do (Boolean b) <- evalM c
     (if b then (evalM t) else (evalM e))

evalErr :: ABE -> (Maybe ABE)
evalErr _ = Nothing -- Replace this with your interpreter

-- Type Derivation Function

typeofM :: ABE -> Maybe TABE
typeofM _ = Nothing

-- Combined interpreter

evalTypeM :: ABE -> Maybe ABE
evalTypeM _ = Nothing

-- Optimizer

optimize :: ABE -> ABE
optimize e = e

interpOptM :: ABE -> Maybe ABE
interpOptM _ = Nothing
