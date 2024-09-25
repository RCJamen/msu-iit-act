-- Ramel Cary B. Jamen
-- 2019-2093

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
--  parseBExpr "x > 1 | x == 1 & b | T"
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

parseBExpr :: String -> BExp
parseBExpr rstr = case parse bexp rstr of
    [(n,"")] -> n
    [(_, out)] -> error ("Unused input " ++ out)
    [] -> error "Invalid input"

------------------------------------------
--
-- <seqcom> ::= <comparse> ";" <seqcom> 
--           | <comparse>
--
-- <comparse> ::= <skip> | <block> | <cblock> | <assign> | <ifchoice> | <wdloop> | <dwloop> | <repuntil> | <forloop>
-- 
-- <skip> ::= "skip"
-- 
-- <block> ::= "begin" <seqcom> "end"
-- 
-- <cblock> ::= "{" <seqcom> "}"
-- 
-- <assign> ::= <identifier> ":=" <expr>
-- 
-- <ifchoice> ::= "if" <bexp> "then" <comparse> "else" <comparse>
--
-- <wdloop> ::= "while" <bexp> "do" <comparse>
--
-- <dwloop> ::= "do" <comparse> "while" <bexp> 
-- 
-- <repuntil> ::= "repeat" <comparse> "until" <bexp> 
-- 
-- <forloop> ::= "for" "(" <comparse> ";" <bexp> ";" <comparse> ")" "do" <comparse> 
-- 
------------------------------------------


data Com
    = Skip
    | Assign String AExp
    | Seq Com Com
    | Coms [Com]
--    | Begin Com
    | If BExp Com Com
    | WhileDo BExp Com
    | DoWhile Com BExp
    | RepUntil Com BExp
    | ForLoop (Com, BExp, Com) Com
    deriving Show
    

sep1 :: Parser Com -> Parser a -> Parser [Com]
sep1 p sep = do x <- p; xs <- many (sep >> p); return (x : xs)

sep0 :: Parser Com -> Parser sep -> Parser [Com]
sep0 p sep = sep1 p sep +++ return []


coms = do lst <- (sep1 comparse (symbol ";")); return (if length lst == 1 then head lst else Coms lst) 
    
seqcom :: Parser Com
seqcom = do t <- comparse; symbol ";"; e <- seqcom; return (Seq t e)
            +++ comparse

comparse :: Parser Com
comparse = skip +++ block +++ cblock +++ assign +++ ifchoice +++ wdloop +++ dwloop +++ repuntil +++ forloop
   

skip :: Parser Com
skip = do symbol "skip"; return Skip

assign :: Parser Com
assign = do id <- identifier; symbol ":="; e <- expr; return (Assign id e)

block :: Parser Com
block = do symbol "begin"; t <- seqcom; symbol "end"; return (Coms [t]) +++ comparse

cblock :: Parser Com
cblock = do symbol "{"; t <- seqcom; symbol "}"; return (Coms [t]) +++ comparse

ifchoice :: Parser Com
ifchoice = do symbol "if"; b <- bexp; symbol "then"; ct <- comparse; symbol "else"; cf <- comparse; return (If b ct cf)

wdloop :: Parser Com
wdloop = do symbol "while"; b <- bexp; symbol "do"; ct <- comparse; return (WhileDo b ct)


dwloop :: Parser Com
dwloop = do symbol "do"; b <- comparse; symbol "while"; ct <- bexp; return (DoWhile b ct)

repuntil :: Parser Com
repuntil = do symbol "repeat"; b <- comparse; symbol "until"; ct <- bexp; return (RepUntil b ct)

forloop :: Parser Com
forloop = do symbol "for"; symbol "("; b <- comparse; symbol ";"; ct <- bexp; symbol ";"; cf <- comparse; symbol ")"; symbol "do"; c <- comparse; return (ForLoop (b, ct, cf) c)


parse_com :: String -> Com
parse_com rstr = case parse seqcom rstr of
    [(n,"")] -> n
    [(_, out)] -> error ("Unused input " ++ out)
    [] -> error "Invalid input"
    
    
parse_coms :: String -> Com
parse_coms rstr = case parse coms rstr of
    [(n,"")] -> n
    [(_, out)] -> error ("Unused input " ++ out)
    [] -> error "Invalid input"
    
    
type Loc = String

data Ins = PUSH Int | ADD | MUL | SUB
         | TRUE | FALSE | EQU | GRT | LST | GTE | LTE | AND | OR | NOT
         | LOAD Loc | STORE Loc | NOOP | BRANCH (Code, Code) | LOOP (Code, Code)
         deriving Show
         
type Code = [Ins] 

tr_a :: AExp -> Code
tr_a (Num n) = [PUSH n]
tr_a (Var x) = [LOAD x]
tr_a (A Add a1 a2) = tr_a a2 ++ tr_a a1 ++ [ADD]
tr_a (A Sub a1 a2) = tr_a a2 ++ tr_a a1 ++ [SUB]
tr_a (A Mul a1 a2) = tr_a a2 ++ tr_a a1 ++ [MUL]


tr_b :: BExp -> Code
tr_b (BVal True) = [TRUE]
tr_b (BVal False) = [FALSE]
tr_b (BVar x) = [LOAD x]
tr_b (Not l1) = tr_b l1 ++ [NOT]
tr_b (R Equ l1 l2) = tr_a l2 ++ tr_a l1 ++ [EQU]
tr_b (R Gt l1 l2) = tr_a l2 ++ tr_a l1 ++ [GRT]
tr_b (R Lt l1 l2) = tr_a l2 ++ tr_a l1 ++ [LST]
tr_b (R Gte l1 l2) = tr_a l2 ++ tr_a l1 ++ [GTE]
tr_b (R Lte l1 l2) = tr_a l2 ++ tr_a l1 ++ [LTE]
tr_b (L And l1 l2) = tr_b l1 ++ tr_b l2 ++ [AND]
tr_b (L Or l1 l2) = tr_b l1 ++ tr_b l2 ++ [OR]


tr_c :: Com -> Code
tr_c (Skip) = [NOOP]
tr_c (Assign m v) = tr_a v ++ [STORE m]
tr_c (Seq c1 c2) = tr_c c1 ++ tr_c c2
tr_c (If b c1 c2) = tr_b b ++ [BRANCH(tr_c c1, tr_c c2)]
tr_c (WhileDo b c) = [LOOP (tr_b b, tr_c c)]
tr_c (DoWhile c b) = [LOOP (tr_c c, tr_b b)]
tr_c (RepUntil c b) = [LOOP (tr_c c, tr_b b)]




         
