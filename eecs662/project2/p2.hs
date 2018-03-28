{-# LANGUAGE GADTs,FlexibleContexts #-}

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

-- Imports for PLIH
import ParserUtils

--
-- An interpreter using substitution and an interpreter using
-- an environment.
--
-- Author: Sharynne Azhar
-- Date: March 27, 2018
-- KUID: 2513206
--

-- BBAE AST and Type Definitions
data TBBAE where
  TNum :: TBBAE
  TBool :: TBBAE
  deriving (Show,Eq)

data BBAE where
  Num :: Int -> BBAE
  Plus :: BBAE -> BBAE -> BBAE
  Minus :: BBAE -> BBAE -> BBAE
  Bind :: String -> BBAE -> BBAE -> BBAE
  Id :: String -> BBAE
  Boolean :: Bool -> BBAE
  And :: BBAE -> BBAE -> BBAE
  Leq :: BBAE -> BBAE -> BBAE
  IsZero :: BBAE -> BBAE
  If :: BBAE -> BBAE -> BBAE -> BBAE
  deriving (Show,Eq)


-- Ast Pretty Printer
pprint :: BBAE -> String
pprint (Num n) = show n
pprint (Boolean b) = show b
pprint (Plus n m) = "(" ++ pprint n ++ " + " ++ pprint m ++ ")"
pprint (Minus n m) = "(" ++ pprint n ++ " - " ++ pprint m ++ ")"
pprint (And n m) = "(" ++ pprint n ++ " && " ++ pprint m ++ ")"
pprint (Leq n m) = "(" ++ pprint n ++ " <= " ++ pprint m ++ ")"
pprint (IsZero m) = "(isZero " ++ pprint m ++ ")"
pprint (If c n m) = "(if " ++ pprint c ++ " then " ++ pprint n ++ " else " ++ pprint m ++ ")"
pprint (Id s) = s
pprint (Bind n v b) = "(bind " ++ n ++ " = " ++ pprint v ++ " in " ++ pprint b ++ ")"


-- Parser
expr :: Parser BBAE
expr = buildExpressionParser opTable term

opTable = [ [ inFix "+" Plus AssocLeft
              , inFix "-" Minus AssocLeft ]
          , [ inFix "<=" Leq AssocLeft
            , preFix "isZero" IsZero ]
          , [ inFix "&&" And AssocLeft ]
          ]

numExpr :: Parser BBAE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

identExpr :: Parser BBAE
identExpr = do i <- identifier lexer
               return (Id i)

bindExpr :: Parser BBAE
bindExpr = do reserved lexer "bind"
              i <- identifier lexer
              reservedOp lexer "="
              v <- expr
              reserved lexer "in"
              e <- expr
              return (Bind i v e)

trueExpr :: Parser BBAE
trueExpr = do i <- reserved lexer "true"
              return (Boolean True)

falseExpr :: Parser BBAE
falseExpr = do i <- reserved lexer "false"
               return (Boolean False)

ifExpr :: Parser BBAE
ifExpr = do reserved lexer "if"
            c <- expr
            reserved lexer "then"
            t <- expr
            reserved lexer "else"
            e <- expr
            return (If c t e)

term = parens lexer expr
       <|> numExpr
       <|> identExpr
       <|> bindExpr
       <|> trueExpr
       <|> falseExpr
       <|> ifExpr


-- Parser invocation
parseBBAEM = parseM expr
parseBBAE = parseString expr
parseBBAEFile = parseFile expr


-- Substitution
subst :: String -> BBAE -> BBAE -> BBAE
subst _ _ (Num n) = (Num n)

subst i v (Plus l r) = (Plus (subst i v l) (subst i v r))

subst i v (Minus l r) = (Minus (subst i v l) (subst i v r))

subst i v (Bind i' v' b') = if (i == i')
                            then (Bind i' (subst i v v') b')
                            else (Bind i' (subst i v v') (subst i v b'))

subst i v (Id i') = if (i == i') then v
                    else (Id i')

subst _ _ (Boolean b) = (Boolean b)

subst i v (And l r) = (And (subst i v l) (subst i v r))

subst i v (Leq l r) = (Leq (subst i v l) (subst i v r))

subst i v (IsZero n) = (IsZero (subst i v n))

subst i v (If c t e) = (If (subst i v c) (subst i v t) (subst i v e))


-- Evaluation Functions
type Env = [(String,BBAE)]
type Cont = [(String,TBBAE)]

evalS :: BBAE -> (Maybe BBAE)
evalS (Num n) = return (Num n)

evalS (Plus l r) = do (Num l') <- (evalS l);
                      (Num r') <- (evalS r);
                      return (Num (l' + r'))

evalS (Minus l r) = do (Num l') <- (evalS l);
                       (Num r') <- (evalS r);
                       if ((l' - r') > 0)
                       then return (Num (l' - r'))
                       else Nothing

evalS (Bind i v b) = do v' <- (evalS v);
                        (evalS (subst i v' b))

evalS (Id id) = Nothing

evalS (Boolean b) = return (Boolean b)

evalS (And l r) = do (Boolean l') <- (evalS l);
                     (Boolean r') <- (evalS r);
                     return (Boolean (l' && r'))

evalS (Leq l r) = do (Num l') <- (evalS l);
                     (Num r') <- (evalS r);
                     return (Boolean (l' <= r'))

evalS (IsZero n) = do (Num n') <- (evalS n);
                      return (Boolean (n' == 0))

evalS (If c t e) = do (Boolean c') <- (evalS c);
                      if c' then (evalS t)
                      else (evalS e)

interpS = evalS . parseBBAE

evalM :: Env -> BBAE -> (Maybe BBAE)
evalM env (Num n) = (Just (Num n))

evalM env (Plus l r) = do (Num l') <- (evalM env l);
                          (Num r') <- (evalM env r);
                          return (Num (l' + r'))

evalM env (Minus l r) = do (Num l') <- (evalM env l);
                           (Num r') <- (evalM env r);
                           if ((l' - r') > 0)
                           then return (Num (l' - r'))
                           else Nothing

evalM env (Bind i v b) = do v' <- (evalM env v);
                            evalM ((i,v'):env) b

evalM env (Id id) = Nothing

evalM env (Boolean b) = (Just (Boolean b))

evalM env (And l r) = do (Boolean l') <- (evalM env l);
                         (Boolean r') <- (evalM env r);
                         return (Boolean (l' && r'))

evalM env (Leq l r) = do (Num l') <- (evalM env l);
                         (Num r') <- (evalM env r);
                         return (Boolean (l' <= r'))

evalM env (IsZero n) = do (Num n') <- (evalM env n);
                          return (Boolean (n' == 0))

evalM env (If c t e) = do (Boolean c') <- (evalM env c);
                          if c' then (evalM env t)
                          else (evalM env e)

interpM = (evalM []) . parseBBAE

testBBAE :: BBAE -> Bool
testBBAE expr = ((evalS expr) == (evalM [] expr))

typeofM :: Cont -> BBAE -> (Maybe TBBAE)
typeofM cont (Num n) = (Just TNum)

typeofM cont (Plus l r) = do l' <- (typeofM cont l);
                             r' <- (typeofM cont r);
                             if l' == TNum && r' == TNum
                             then return TNum
                             else Nothing

typeofM cont (Minus l r) = do l' <- (typeofM cont l);
                              r' <- (typeofM cont r);
                              if l' == TNum && r' == TNum
                              then return TNum
                              else Nothing

typeofM cont (Bind i v b) = do v' <- typeofM cont v;
                               typeofM ((i,v'):cont) b

typeofM cont (Id id) = (lookup id cont)

typeofM cont (Boolean b) = Just TBool

typeofM cont (And l r) = do TBool <- (typeofM cont l);
                            TBool <- (typeofM cont r);
                            return TBool

typeofM cont (Leq l r) = do TNum <- (typeofM cont l);
                            TNum <- (typeofM cont r);
                            return TBool

typeofM cont (IsZero n) = do TNum <- (typeofM cont n);
                             return TBool

typeofM cont (If c t e) = do c' <- (typeofM cont c);
                             t' <- (typeofM cont t);
                             e' <- (typeofM cont e);
                             if t' == e' then return t'
                             else Nothing

evalT :: BBAE -> (Maybe BBAE)
evalT expr = do expr' <- (typeofM [] expr);
                (evalM [] expr)


-- Testing
-- Arbitrary AST Generator

instance Arbitrary BBAE where
  arbitrary =
    sized $ \n -> genBBAE (rem n 10) []

genNum =
  do t <- choose (0,100)
     return (Num t)

genId e =
  do n <- elements e
     return (Id n)

genPlus n e =
  do s <- genBBAE n e
     t <- genBBAE n e
     return (Plus s t)

genMinus n e =
  do s <- genBBAE n e
     t <- genBBAE n e
     return (Minus s t)

genAnd n e =
  do s <- genBBAE n e
     t <- genBBAE n e
     return (And s t)

genLeq n e =
  do s <- genBBAE n e
     t <- genBBAE n e
     return (Leq s t)

genIsZero n e =
  do s <- genBBAE n e
     return (IsZero s)

genIf n e =
  do s <- genBBAE n e
     t <- genBBAE n e
     u <- genBBAE n e
     return (If s t u)

genBind n e =
  do i <- genName
     v <- genBBAE n e
     b <- genBBAE n (i:e)
     return (Bind i v b)

genName =
  do i <- choose ('v','z')
     return [i]

genBool =
  do t <- choose (True,False)
     return (Boolean t)

genBBAE :: Int -> [String] -> Gen BBAE
genBBAE 0 e =
  do term <- oneof (case e of
                      [] -> [genNum,genBool]
                      _ -> [genNum
                           , genBool
                           , (genId e)])
     return term
genBBAE n e =
  do term <- oneof [genNum
                   , (genPlus (n-1) e)
                   , (genMinus (n-1) e)
                   , (genAnd (n-1) e)
                   , (genLeq (n-1) e)
                   , (genIsZero (n-1) e)
                   , (genIf (n-1) e)]
     return term

-- QuickCheck

testParser :: Int -> IO ()
testParser n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> parseBBAE (pprint t) == t)

testEval :: Int -> IO ()
testEval n = verboseCheckWith stdArgs {maxSuccess=n}
  (\t -> testBBAE t)

testEvalM :: Int -> IO ()
testEvalM n = verboseCheckWith stdArgs {maxSuccess=n}
  (\t -> (interpM $ pprint t) == (evalM [] t))

testTypedEval :: Int -> IO ()
testTypedEval n = verboseCheckWith stdArgs {maxSuccess=n}
  (\t -> case typeofM [] t of
           (Just _) -> ((evalM []) . parseBBAE . pprint) t == (evalM [] t)
           Nothing -> True)
