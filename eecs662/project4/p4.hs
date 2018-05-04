--
-- An interpreter using type checking
--
-- Author: Sharynne Azhar
-- Date: May 04, 2018
-- KUID: 2513206
--

{-# LANGUAGE GADTs #-}

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Token

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
  ClosureV :: String -> TFBAE -> FBAE -> Env -> FBAEVal
  deriving (Show,Eq)

-- Environment for statically scoped eval
type Env = [(String,FBAEVal)]

-- Substitution
subst :: String -> FBAE -> FBAE -> FBAE
subst i v (Num n)         = Num n
subst i v (Boolean b)     = Boolean b
subst i v (Plus l r)      = Plus (subst i v l) (subst i v r)
subst i v (Minus l r)     = Minus (subst i v l) (subst i v r)
subst i v (Mult l r)      = Mult (subst i v l) (subst i v r)
subst i v (Div l r)       = Div (subst i v l) (subst i v r)
subst i v (And l r)       = And (subst i v l) (subst i v r)
subst i v (Or l r)        = Or (subst i v l) (subst i v r)
subst i v (Leq l r)       = Leq (subst i v l) (subst i v r)
subst i v (IsZero x)      = IsZero (subst i v x)
subst i v (If c t e)      = If (subst i v c) (subst i v t) (subst i v e)
subst i v (App l r)       = App (subst i v l) (subst i v r)
subst i v (Lambda i' t b) = Lambda i' t (subst i v b)
subst i v (Fix x)         = Fix (subst i v x)
subst i v (Id x)          = if (i == x) then v else (Id x)
subst i v (Bind i' v' b') = if (i == i') then (Bind i' (subst i v v') b')
                            else (Bind i' (subst i v v') (subst i v b'))

-- Statically scoped eval
evalM :: Env -> FBAE -> (Maybe FBAEVal)
evalM env (Num n) = return (NumV n)
evalM evn (Boolean b) = return (BooleanV b)

evalM env (Plus l r) = do { NumV l' <- evalM env l;
                            NumV r' <- evalM env r;
                            return (NumV (l' + r')) }

evalM env (Minus l r) = do { NumV l' <- evalM env l;
                             NumV r' <- evalM env r;
                             if (l' > r') then return (NumV (l' - r'))
                             else Nothing }

evalM env (Mult l r) = do { NumV l' <- evalM env l;
                            NumV r' <- evalM env r;
                            return (NumV (l' * r')) }

evalM env (Div l r) = do { NumV l' <- evalM env l;
                           NumV r' <- evalM env r;
                           if (r' == 0) then Nothing
                           else return (NumV (l' * r')) }

evalM env (And l r) = do { BooleanV l' <- evalM env l;
                           BooleanV r' <- evalM env r;
                           return (BooleanV (l' && r')) }

evalM env (Or l r) = do { BooleanV l' <- evalM env l;
                          BooleanV r' <- evalM env r;
                          return (BooleanV (l' || r')) }

evalM env (Leq l r) = do { NumV l' <- evalM env l;
                           NumV r' <- evalM env r;
                           return (BooleanV (l' <= r')) }

evalM env (IsZero x) = do { NumV x' <- evalM env x;
                            return (BooleanV (x' == 0)) }

evalM env (If c t e) = do { (BooleanV c') <- (evalM env c);
                            if c' then (evalM env t)
                            else (evalM env e) }

evalM env (App f a) = do { (ClosureV i t b e) <- (evalM env f);
                           a' <- (evalM env a);
                           evalM ((i, a'):e) b }

evalM env (Lambda i t b) = return (ClosureV i t b env)

evalM env (Fix f) = do { (ClosureV i t b e) <- (evalM env f);
                         evalM env (subst i (Fix (Lambda i t b)) b) }

evalM env (Id id) = (lookup id env)

evalM env (Bind i v b) = do { a' <- (evalM env v);
                              (evalM ((i,a'):env) b) }

-- Type inference function
type Cont = [(String,TFBAE)]

typeofM :: Cont -> FBAE -> (Maybe TFBAE)
typeofM cont (Num n) = return TNum
typeofM cont (Boolean b) = return TBool

typeofM cont (Plus l r) = do { l' <- (typeofM cont l);
                               r' <- (typeofM cont r);
                               if (l' == TNum && r' == TNum) then return TNum
                               else Nothing }

typeofM cont (Minus l r) = do { l' <- (typeofM cont l);
                                r' <- (typeofM cont r);
                                if (l' == TNum && r' == TNum) then return TNum
                                else Nothing }

typeofM cont (Mult l r) = do { l' <- (typeofM cont l);
                               r' <- (typeofM cont r);
                               if (l' == TNum && r' == TNum) then return TNum
                               else Nothing }

typeofM cont (Div l r) = do { l' <- (typeofM cont l);
                              r' <- (typeofM cont r);
                              if (l' == TNum && r' == TNum) then return TNum
                              else Nothing }

typeofM cont (And l r) = do { l' <- (typeofM cont l);
                              r' <- (typeofM cont r);
                              if (l' == TBool && r' == TBool) then return TBool
                              else Nothing }

typeofM cont (Or l r) = do { l' <- (typeofM cont l);
                             r' <- (typeofM cont r);
                             if (l' == TBool && r' == TBool) then return TBool
                             else Nothing }

typeofM cont (Leq l r) = do { l' <- (typeofM cont l);
                              r' <- (typeofM cont r);
                              if (l' == TNum && r' == TNum) then return TNum
                              else Nothing }

typeofM cont (IsZero x) = do { x' <- (typeofM cont x);
                               if (x' == TNum) then return TBool
                               else Nothing }

typeofM cont (If c t e) = do { c' <- (typeofM cont c);
                               t' <- (typeofM cont t);
                               e' <- (typeofM cont e);
                               if (c' == TBool) then return TBool
                               else Nothing }

typeofM cont (App f a) = do { a' <- (typeofM cont a);
                              (d' :->: r') <- (typeofM cont f);
                              if (a' == d') then return r'
                              else Nothing }

typeofM cont (Lambda i t b) = do { b' <- (typeofM ((i,t):cont) b);
                                   return (t :->: b') }

typeofM cont (Fix f) = do { (f' :->: r') <- (typeofM cont f);
                            return r' }

typeofM cont (Id id) = (lookup id cont)

typeofM cont (Bind i v b) = do { v' <- (typeofM cont v);
                                 (typeofM ((i,v'):cont) b) }

-- Interpreter
interp :: FBAE -> (Maybe FBAEVal)
interp expr = do { typeofM [] expr;
                   evalM [] expr }

