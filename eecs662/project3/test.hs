{-# LANGUAGE GADTs,FlexibleContexts #-}

-- Imports for Monads
import Control.Monad

-- Imports for Parser
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

-- Imports for PLIH
import ParserUtils

-- Import p3 stuff
import P3

-- Parser magic
expr :: Parser CFAE
expr = buildExpressionParser opTable term

opTable = [ [ inFix "+" Plus AssocLeft ,
              inFix "-" Minus AssocLeft ] ]

numExpr :: Parser CFAE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

lambdaExpr :: Parser CFAE
lambdaExpr = do reserved lexer "lambda"
                i <- identifier lexer
                reserved lexer "in"
                e <- expr
                return (Lambda i e)

appExpr :: Parser CFAE
appExpr = do reserved lexer "app"
             e1 <- expr
             e2 <- expr
             return (App e1 e2)

identExpr :: Parser CFAE
identExpr = do i <- identifier lexer
               return (Id i)

ifExpr :: Parser CFAE
ifExpr = do reserved lexer "if"
            c <- expr
            reserved lexer "then"
            t <- expr
            reserved lexer "else"
            e <- expr
            return (If0 c t e)

term = parens lexer expr
       <|> numExpr
       <|> lambdaExpr
       <|> appExpr
       <|> identExpr
       <|> ifExpr

-- Parser invocation
parseCFAE = parseString expr


expr' :: Parser CFBAE
expr' = buildExpressionParser opTable' term'

opTable' = [ [ inFix "+" Plus' AssocLeft ,
               inFix "-" Minus' AssocLeft ] ]

numExpr' :: Parser CFBAE
numExpr' = do i <- integer lexer
              return (Num' (fromInteger i))

lambdaExpr' :: Parser CFBAE
lambdaExpr' = do reserved lexer "lambda"
                 i <- identifier lexer
                 reserved lexer "in"
                 e <- expr'
                 return (Lambda' i e)

appExpr' :: Parser CFBAE
appExpr' = do reserved lexer "app"
              e1 <- expr'
              e2 <- expr'
              return (App' e1 e2)

bindExpr' :: Parser CFBAE
bindExpr' = do reserved lexer "bind"
               i <- identifier lexer
               reservedOp lexer "="
               v <- expr'
               reserved lexer "in"
               e <- expr'
               return (Bind' i v e)

identExpr' :: Parser CFBAE
identExpr' = do i <- identifier lexer
                return (Id' i)

ifExpr' :: Parser CFBAE
ifExpr' = do reserved lexer "if"
             c <- expr'
             reserved lexer "then"
             t <- expr'
             reserved lexer "else"
             e <- expr'
             return (If0' c t e)

term' = parens lexer expr'
        <|> numExpr'
        <|> lambdaExpr'
        <|> appExpr'
        <|> bindExpr'
        <|> identExpr'
        <|> ifExpr'

-- Parser invocation
parseCFBAE = parseString expr'

-- P3 Tests
interpDyn :: String -> (Maybe CFAE)
interpDyn = (evalDynCFAE []) . parseCFAE

interpStat :: String -> (Maybe CFAEValue)
interpStat = (evalStatCFAE []) . parseCFAE

interpCFBAE :: String -> (Maybe CFAEValue)
interpCFBAE = (evalCFBAE []) . parseCFBAE
