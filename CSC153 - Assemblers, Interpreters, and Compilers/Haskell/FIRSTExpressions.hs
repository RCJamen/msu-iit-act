-- Ramel Cary B. Jamen
-- 2019-2093

import Parsing 
------------------------------------------
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

data AExp
    = Add AExp AExp 
    | Mul AExp AExp
    | Sub AExp AExp
    | Num Int
    | Var String
    | A AExp
    deriving Show

expr :: Parser AExp
expr = do t <- term
          do symbol "+"; e <- expr; return (Add t e)
            +++ do symbol "-"; e <- expr; return (Sub t e)
             +++ return t

term :: Parser AExp
term = do f <- factor 
          do symbol "*"; t <- term; return (Mul f t)
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
parseAExpr xs = A (parseAExpr' xs) 

parseAExpr' :: String -> AExp
parseAExpr' xs = case (parse expr xs) of
    [(n,[])] -> n
    [(_,out)] -> error ("Unused input " ++ out)
    [] -> error "Invalid input"

aeval :: AExp -> Int
aeval (Num n) = n 
aeval (Add a1 a2) = aeval a1 + aeval a2
aeval (Mul a1 a2) = aeval a1 * aeval a2
aeval (Sub a1 a2) = aeval a1 - aeval a2


------------------------------------------
--
--  rexp ::= equal | lessthan | greaterthan | lessthanequal | greaterthanequal | notequal
--  equal            ::= aexpr '==' aexpr
--  lessthan         ::= aexpr '<'  aexpr
--  greaterthan      ::= aexpr '>'  aexpr 
--  lessthanequal    ::= aexpr '<=' aexpr
--  greaterthanequal ::= aexpr '>=' aexpr
--  notequal         ::= aexpr '!=' aexpr
--
--  USAGE:
--  parseRExpr "x + 1 > y" 
--      GreaterThan (Add (Var "x") (Num 1)) (Var "y")
--

data RExp
    = Equal AExp AExp
    | LessThan AExp AExp
    | GreaterThan AExp AExp
    | LessThanEqual AExp AExp
    | GreaterThanEqual AExp AExp
    | NotEqual AExp AExp
    deriving Show
    
rexp :: Parser RExp
rexp = equal +++ lessthan +++ greaterthan +++ lessthanequal +++ greaterthanequal +++ notequal

equal :: Parser RExp
equal = do l1 <- expr; symbol "=="; l2 <- expr; return (Equal l1 l2)

lessthan :: Parser RExp
lessthan = do l1 <- expr; symbol "<"; l2 <- expr; return (LessThan l1 l2)   
    
greaterthan :: Parser RExp
greaterthan = do l1 <- expr; symbol ">"; l2 <- expr; return (GreaterThan l1 l2)   
    
lessthanequal :: Parser RExp
lessthanequal = do l1 <- expr; symbol "<="; l2 <- expr; return (LessThanEqual l1 l2)   
    
greaterthanequal :: Parser RExp
greaterthanequal = do l1 <- expr; symbol ">="; l2 <- expr; return (GreaterThanEqual l1 l2)

notequal :: Parser RExp
notequal = do l1 <- expr; symbol "!="; l2 <- expr; return (NotEqual l1 l2)   

reval :: RExp -> Bool
reval (Equal l1 l2) = aeval l1 == aeval l2
reval (LessThan l1 l2) = aeval l1 < aeval l2
reval (GreaterThan l1 l2) = aeval l1 > aeval l2
reval (LessThanEqual l1 l2) = aeval l1 <= aeval l2
reval (GreaterThanEqual l1 l2) = aeval l1 >= aeval l2
reval (NotEqual l1 l2) = aeval l1 /= aeval l2

parseRExpr :: String -> RExp
parseRExpr rstr = case parse rexp rstr of 
    [(n,"")] -> n
    [(_, out)] -> error ("Unused input " ++ out)
    [] -> error "Invalid input"

rexresolve :: String -> Bool
rexresolve = reval . parseRExpr

------------------------------------------
--
-- <bexp> ::= <bterm> { '|' <bterm> }
-- <bterm> ::= <bfactor> { '&' <bfactor> }
-- <bfactor> ::= '!' <bfactor>
--            | '(' <bexp> ')'
--            | <bterm>
--            | <bval>
-- <bval> ::= 'BVal' ('True' | 'False')
-- <bvar> ::= identifier
--
--  USAGE:
--  parseBExpr "T | T"
--    BOr (BVal True) (BVal True)
--
------------------------------------------
data BExp
    = BOr BExp BExp
    | BAnd BExp BExp
    | BNot BExp
    | BVal Bool
    | BVar String
    deriving Show
    
bexp :: Parser BExp
bexp = do t <- bterm
          do symbol "|"; e <- bexp;  return (BOr t e)
            +++ return t

bterm :: Parser BExp
bterm = do f <- bfactor
           do symbol "&"; t <- bterm; return (BAnd f t)
             +++ return f

bfactor :: Parser BExp
bfactor = do symbol "!"; f <- bfactor; return (BNot f)
            +++ do symbol "("; e <- bexp; symbol ")"; return e
             +++ bval
             +++ bvar
             
bval :: Parser BExp
bval = do symbol "F"; return (BVal False)
        +++ do symbol "T"; return (BVal True)

bvar :: Parser BExp
bvar = do v <- identifier; return (BVar v)

parseBExpr :: String -> BExp
parseBExpr bstr = case parse bexp bstr of 
    [(n,"")] -> n
    [(_, out)] -> error ("Unused input " ++ out)
    [] -> error "Invalid input"

beval :: BExp -> Bool
beval (BVal b) = b
beval (BOr b1 b2) = beval b1 || beval b2
beval (BAnd b1 b2) = beval b1 && beval b2
beval (BNot b) = False

resolve :: String -> Bool
resolve = beval . parseBExpr

------------------------------------------
--
--  <logical> ::= <logOr> | <logAnd> | <logNeg>
--  <logOr> ::= <rexp> "||" <rexp>
--  <logAnd> ::= <rexp> "&&" <rexp>
--  <logNeg> ::= "!" "(" <rexp> ")"
--
--  <exp> ::= "R" <rexp> | "B" <bexp> | "L" <logical>
--
--  USAGE:  
--  parseExp "x + 1 > y || y == x * 2"
--      L (OR (R (GreaterThan (Add (Var "x") (Num 1)) (Var "y"))) (R (Equal (Var "y") (Mul (Var "x") (Num 2)))))  
--
--
------------------------------------------
data LExp
    = OR Exp Exp
    | AND Exp Exp
    | NEG Exp 
    deriving Show
    
logical :: Parser LExp
logical = logOr +++ logAnd +++ logNeg

logOr :: Parser LExp
logOr = do l1 <- logical; symbol "||"; l2 <- logical; return (OR l1 l2)

logAnd :: Parser LExp
logAnd = do l1 <- logical; symbol "&&"; l2 <- logical; return (AND l1 l2)

logNeg :: Parser LExp
logNeg = do symbol "!"; symbol "("; l1 <- logical; symbol ")"; return (NEG l1)
    
parseBExpr' :: String -> LExp
parseBExpr' rstr = case parse logical rstr of 
    [(n,"")] -> n
    [(_, out)] -> error ("Unused input " ++ out)
    [] -> error "Invalid input"    





data Exp
    = L LExp
    | R RExp
    | B BExp
    deriving Show
   
expParser :: Parser Exp
expParser = tryR +++ tryB +++ tryL
    where
        tryR = R <$> rexp
        tryB = B <$> bexp
        tryL = L <$> logical
    
parseExp :: String -> Exp
parseExp xs =  (parseExpr' xs) 

parseExpr' :: String -> Exp
parseExpr' rstr = case parse expParser rstr of 
    [(n,"")] -> n
    [(_, out)] -> error ("Unused input " ++ out)
    [] -> error "Invalid input"    

------------------------------------------
--
--"If x > 1 then x := 1 else skip"
--"If (R (GreaterThan (Var "x") (Num 1))) (Assign "x" (Num 1)) Skip"
--
--
data Com
    = Skip
    | Assign String AExp
    | Seq Com Com
    | If Exp Com Com
    | While Exp Com
    deriving Show










