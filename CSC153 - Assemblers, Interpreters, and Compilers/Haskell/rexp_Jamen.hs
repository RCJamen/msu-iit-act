import Parsing 

data AExp
    = Add AExp AExp 
    | Mul AExp AExp
    | Sub AExp AExp
    | Num Int
    | Var String
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
parseAExpr xs = case (parse expr xs) of
    [(n,[])] -> n
    [(_,out)] -> error ("Unused input " ++ out)
    [] -> error "Invalid input"

aeval :: AExp -> Int
aeval (Num n) = n
aeval (Add a1 a2) = aeval a1 + aeval a2
aeval (Mul a1 a2) = aeval a1 * aeval a2
aeval (Sub a1 a2) = aeval a1 - aeval a2

------------------------------------------
-- Ramel Cary B. Jamen

-- bnf

-- rexp ::= equal | lessthan | greaterthan | lessthanequal | greaterthanequal | notequal

-- equal            ::= aexpr '==' aexpr
-- lessthan         ::= aexpr '<'  aexpr
-- greaterthan      ::= aexpr '>'  aexpr 
-- lessthanequal    ::= aexpr '<=' aexpr
-- greaterthanequal ::= aexpr '>=' aexpr
-- notequal         ::= aexpr '!=' aexpr

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




