-- Ramel Cary B. Jamen
-- 2019-2093
module Expressions where

import Parsing

------------------------------------------
--  BNF Grammar
--
--  expr ::= term { ("+" | "-") term }
--  term ::= factor { "*" factor }
--  factor ::= "(" expr ")" | number | var
--  number ::= natural
--  var ::= string
--
--  USAGE:
--  parseAExpr "1 + x"
--      A (Add (Num 1) (Var "x"))
--
------------------------------------------

data AOp
    = Add | Mul | Sub
    deriving Show

data AExp
    = Num Int
    | Var String
    | A AOp AExp AExp
    deriving Show


expr :: Parser AExp
expr = do t <- term
          do symbol "+"; e <- expr; return (A Add t e)
            +++ do symbol "-"; e <- expr; return (A Sub t e)
             +++ return t

term :: Parser AExp
term = do f <- factor
          do symbol "*"; t <- term; return (A Mul f t)
             +++ return f

factor :: Parser AExp
factor = do symbol "("
            e <- expr; symbol ")"; return e
            +++ number
            +++ var

number :: Parser AExp
number = do x <- natural; return (Num x)

var :: Parser AExp
var = do v <- identifier; return (Var v)


parseAExpr :: String -> AExp
parseAExpr xs = case (parse expr xs) of
    [(n,[])] -> n
    [(_,out)] -> error ("Unused input " ++ out)
    [] -> error "Invalid input"

aeval :: AExp -> Int
aeval (Num n) = n
aeval (A Add a1 a2) = aeval a1 + aeval a2
aeval (A Mul a1 a2) = aeval a1 * aeval a2
aeval (A Sub a1 a2) = aeval a1 - aeval a2

------------------------------------------
-- BNF Grammar
--
--  <exp> ::= <term> "|" <exp> | <term>
--  <term> ::= <factor> "&" <term> | <factor>
--  <factor> ::= <nbval> | <bval> | <bvar>
--  <bval> ::= <bpar> | "F" | "T" | <rexp>
--  <nbval> ::= "!" <factor>
--  <tval> ::= "F" | "T"
--  <bpar> ::= "(" <exp> ")"
--  <bvar> ::= <identifier>
--  <rexp> ::= <eq> | <lt> | <gt> | <lteq> | <gteq> | <noteq>
--  <eq> ::= <expr> "==" <expr>
--  <lt> ::= <expr> "<" <expr>
--  <gt> ::= <expr> ">" <expr>
--  <lteq> ::= <expr> "<=" <expr>
--  <gteq> ::= <expr> ">=" <expr>
--  <noteq> ::= <expr> "!=" <expr>
--
--  USAGE:
--  parse_bexp "x > 1 | x == 1 & b | T"
--      L Or (R Gt (Var "x") (Num 1)) (L Or (L And (R Equ (Var "x") (Num 1)) (BVar "b")) (BVal True))
--
------------------------------------------
data LOp
    = Or | And
    deriving Show

data ROp
    = Equ | Lt | Gt | Lte | Gte | Ne
    deriving Show

data BExp
    = BVal Bool
    | BVar String
    | Not BExp
    | R ROp AExp AExp
    | L LOp BExp BExp
    deriving Show

bexp :: Parser BExp
bexp = (do t <- bterm; symbol "|"; e <- bexp;  return (L Or t e)) +++ bterm

bterm :: Parser BExp
bterm = (do f <- bfactor; symbol "&"; t <- bterm; return (L And f t)) +++ bfactor

bfactor :: Parser BExp
bfactor = nbval +++ bval +++ bvar

bval :: Parser BExp
bval = bpar +++ tval +++ rexp

nbval :: Parser BExp
nbval = do symbol "!"; f <- bfactor; return (Not f)

tval :: Parser BExp
tval = (do symbol "F"; return (BVal False)) +++ (do symbol "T"; return (BVal True))

bpar :: Parser BExp
bpar = do symbol "("; e <- bexp; symbol ")"; return e

bvar :: Parser BExp
bvar = do v <- identifier; return (BVar v)

rexp :: Parser BExp
rexp = eq +++ lt +++ gt +++ lteq +++ gteq +++ noteq

eq :: Parser BExp
eq = do l1 <- expr; symbol "=="; l2 <- expr; return (R Equ l1 l2)

lt :: Parser BExp
lt = do l1 <- expr; symbol "<"; l2 <- expr; return (R Lt l1 l2)

gt :: Parser BExp
gt = do l1 <- expr; symbol ">"; l2 <- expr; return (R Gt l1 l2)

lteq :: Parser BExp
lteq = do l1 <- expr; symbol "<="; l2 <- expr; return (R Lte l1 l2)

gteq :: Parser BExp
gteq = do l1 <- expr; symbol ">="; l2 <- expr; return (R Gte l1 l2)

noteq :: Parser BExp
noteq = do l1 <- expr; symbol "!="; l2 <- expr; return (R Ne l1 l2)

parse_bexp :: String -> BExp
parse_bexp rstr = case parse bexp rstr of
    [(n,"")] -> n
    [(_, out)] -> error ("Unused input " ++ out)
    [] -> error "Invalid input"
    
------------------------------------------
--
-- <seqcom> ::= <comparse> ";" <seqcom> 
--           | <comparse>
--
-- <comparse> ::= <skip> 
--             | <assign> 
--             | <loop> 
--             | <choice>
--
-- <skip> ::= "skip"
--
-- <assign> ::= <identifier> ":=" <expr>
--
-- <choice> ::= "if" <bexp> "then" <comparse> "else" <comparse>
--
-- <loop> ::= "while" <bexp> "do" <comparse>
--
--
------------------------------------------
data Com
    = Skip
    | Assign String AExp
    | Seq Com Com
    | Coms [Com]
    | If BExp Com Com
    | While BExp Com
    | Begin Com
    deriving Show
    
seqcom :: Parser Com
seqcom = do t <- comparse; symbol ";"; do e <- seqcom; return (Seq t e)
	        +++ do c <- comparse; symbol ";"; return c  
            +++ comparse

comparse :: Parser Com
comparse = begin +++ skip +++ assign +++ loop +++ choice 
    
begin :: Parser Com
begin = 

skip :: Parser Com
skip = do symbol "skip"; return Skip

assign :: Parser Com
assign = do id <- identifier; symbol ":="; e <- expr; return (Assign id e)

choice :: Parser Com
choice = do symbol "if"; b <- bexp; symbol "then"; ct <- comparse; symbol "else"; cf <- comparse; return (If b ct cf)

loop :: Parser Com
loop = do symbol "while"; b <- bexp; symbol "do"; ct <- comparse; return (While b ct)

parse_com :: String -> Com
parse_com rstr = case parse seqcom rstr of
    [(n,"")] -> n
    [(_, out)] -> error ("Unused input " ++ out)
    [] -> error "Invalid input"
