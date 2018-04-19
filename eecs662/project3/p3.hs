--
-- An interpreter using scoped functions and elaboration.
--
-- Author: Sharynne Azhar
-- Date: April 19, 2018
-- KUID: 2513206
--

{-# LANGUAGE GADTs,FlexibleContexts #-}

module P3 where

-- Imports for Monads
import Control.Monad

-- Imports for Parser
import ParserUtils


----------------------
-- Type Definitions --
----------------------

type Env = [(String,CFAE)]
type Env' = [(String,CFAEValue)]


----------------------
-- CFAE Definitions --
----------------------

data CFAE where
  Num :: Int -> CFAE
  Plus :: CFAE -> CFAE -> CFAE
  Minus :: CFAE -> CFAE -> CFAE
  Lambda :: String -> CFAE -> CFAE
  App :: CFAE -> CFAE -> CFAE
  Id :: String -> CFAE
  If0 :: CFAE -> CFAE -> CFAE -> CFAE
  deriving (Show,Eq)


evalDynCFAE :: Env -> CFAE -> (Maybe CFAE)
evalDynCFAE env (Num n) = return (Num n)

evalDynCFAE env (Plus l r) = do { (Num l') <- evalDynCFAE env l;
                                  (Num r') <- evalDynCFAE env r;
                                  return (Num (l' + r')) }

evalDynCFAE env (Minus l r) = do { (Num l') <- evalDynCFAE env l;
                                   (Num r') <- evalDynCFAE env r;
                                   return (Num (l' - r')) }

evalDynCFAE env (Lambda i b) = return (Lambda i b)

evalDynCFAE env (App f a) = do { (Lambda i b) <- evalDynCFAE env f;
                                 a' <- evalDynCFAE env a;
                                 evalDynCFAE ((i, a'):env) b }

evalDynCFAE env (Id id) = do { v <- (lookup id env);
                               return v }

evalDynCFAE env (If0 c t e) = do { (Num c') <- evalDynCFAE env c;
                                   if (c' == 0) then (evalDynCFAE env t)
                                   else (evalDynCFAE env e) }

---------------------------
-- CFAEValue Definitions --
---------------------------

data CFAEValue where
  NumV :: Int -> CFAEValue
  ClosureV :: String -> CFAE -> Env' -> CFAEValue
  deriving (Show,Eq)


evalStatCFAE :: Env' -> CFAE -> (Maybe CFAEValue)
evalStatCFAE env (Num n) = return (NumV n)

evalStatCFAE env (Plus l r) = do { (NumV l') <- (evalStatCFAE env l);
                                   (NumV r') <- (evalStatCFAE env r);
                                   return (NumV (l' + r')) }

evalStatCFAE env (Minus l r) = do { (NumV l') <- (evalStatCFAE env l);
                                    (NumV r') <- (evalStatCFAE env r);
                                    return (NumV (l' - r')) }

evalStatCFAE env (Lambda i b) = return (ClosureV i b env)

evalStatCFAE env (App f a) = do { (ClosureV i b e) <- (evalStatCFAE env f);
                                  a' <- (evalStatCFAE env a);
                                  evalStatCFAE ((i, a'):e) b }

evalStatCFAE env (Id id) = do { v <- (lookup id env);
                                return v }

evalStatCFAE env (If0 c t e) = do { (NumV c') <- (evalStatCFAE env c);
                                    if (c' == 0) then (evalStatCFAE env t)
                                    else (evalStatCFAE env e) }


-----------------------
-- CFBAE Definitions --
-----------------------

data CFBAE where
  Num' :: Int -> CFBAE
  Plus' :: CFBAE -> CFBAE -> CFBAE
  Minus' :: CFBAE -> CFBAE -> CFBAE
  Lambda' :: String -> CFBAE -> CFBAE
  App' :: CFBAE -> CFBAE -> CFBAE
  Bind' :: String -> CFBAE -> CFBAE -> CFBAE
  Id' :: String -> CFBAE
  If0' :: CFBAE -> CFBAE -> CFBAE -> CFBAE
  deriving (Show,Eq)


elabCFBAE :: CFBAE -> CFAE
elabCFBAE (Num' n) = (Num n)
elabCFBAE (Plus' l r) = (Plus (elabCFBAE l) (elabCFBAE r))
elabCFBAE (Minus' l r) = (Minus (elabCFBAE l) (elabCFBAE r))
elabCFBAE (Lambda' i b) = (Lambda i (elabCFBAE b))
elabCFBAE (App' f a) = (App (elabCFBAE f) (elabCFBAE a))
elabCFBAE (Bind' i v b) = (App (Lambda i (elabCFBAE b)) (elabCFBAE v))
elabCFBAE (Id' id) = (Id id)
elabCFBAE (If0' c t e) = (If0 (elabCFBAE c) (elabCFBAE t) (elabCFBAE e))

evalCFBAE :: Env' -> CFBAE -> (Maybe CFAEValue)
evalCFBAE env expr = evalStatCFAE env (elabCFBAE expr)
