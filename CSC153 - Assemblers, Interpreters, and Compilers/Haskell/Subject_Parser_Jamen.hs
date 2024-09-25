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

-----------------------------------------

data Ins
    = PUSH Int 
    | ADD
    | SUB
    | MUL
    deriving Show

type Code = [Ins]

ins :: Parser Ins
ins = insPush +++ insAdd +++ insSub +++ insMul

insPush :: Parser Ins
insPush = do symbol "PUSH"; x <- natural; return (PUSH x)

insAdd :: Parser Ins
insAdd = do symbol "ADD"; return ADD

insSub :: Parser Ins
insSub = do symbol "SUB"; return SUB

insMul :: Parser Ins
insMul = do symbol "MUL"; return MUL

code :: Parser [Ins]
code = do i <- ins; symbol ";"; c <- code; return (i:c)
    +++ do i <- ins; return [i]

parseCode :: String -> Code
parseCode xs = case (parse code xs) of
    [(n,"")] -> n
    [(_, out)] -> error ("Unused input " ++ out)
    [] -> error "Invalid input"
    
    
------------------------------------------

to_code :: AExp -> Code
to_code (Num n) = [PUSH n]
to_code (Add t u) = to_code t ++ to_code u ++ [ADD]
to_code (Sub t u) = to_code t ++ to_code u ++ [SUB]
to_code (Mul t u) = to_code t ++ to_code u ++ [MUL]

expToCode :: String -> Code
expToCode xs = to_code (parseAExpr xs)

 ------------------------------------------

type State = (Code, Stack)
type Stack = [Int]

initstate :: Code -> State
initstate lst = (lst,[])

evalNext :: State -> State
evalNext (PUSH x:is, stack) = (is, x:stack)
evalNext (ADD:is, x:y:stack) = (is, (x + y):stack)
evalNext (MUL:is, x:y:stack) = (is, (x * y):stack)
evalNext (i:is, stack) = (is, stack)

evalAll :: State -> State
evalAll ([], stack) = ([], stack)
evalAll ((PUSH x):is, stack) = evalAll (is, x:stack)
evalAll (ADD:is, x:y:stack) = evalAll (is, (x + y):stack)
evalAll (MUL:is, x:y:stack) = evalAll (is, (x * y):stack)
evalAll (_:is, stack) = evalAll (is, stack)

------------------------------------------
-- Ramel Cary B. Jamen

-- <bexp> ::= <bterm> { '|' <bterm> }

-- <bterm> ::= <bfactor> { '&' <bfactor> }

-- <bfactor> ::= '!' <bfactor>
--            | '(' <bexp> ')'
--            | <bterm>
--            | <bval>

-- <bval> ::= 'BVal' ('True' | 'False')

data BExp
    = Or BExp BExp
    | And BExp BExp
    | Not BExp
    | BVal Bool
    deriving Show

bexp :: Parser BExp
bexp = do t <- bterm
          do symbol "|"; e <- bexp;  return (Or t e)
            +++ return t

bterm :: Parser BExp
bterm = do f <- bfactor
           do symbol "&"; t <- bterm; return (And f t)
             +++ return f

bfactor :: Parser BExp
bfactor = do symbol "!"; f <- bfactor; return (Not f)
            +++ do symbol "("; e <- bexp; symbol ")"; return e
             +++ bval
             
bval :: Parser BExp
bval = do symbol "T"; return (BVal True)
        +++ do symbol "F"; return (BVal False)

parseBExpr :: String -> BExp
parseBExpr bstr = case parse bexp bstr of 
    [(n,"")] -> n
    [(_, out)] -> error ("Unused input " ++ out)
    [] -> error "Invalid input"

beval :: BExp -> Bool
beval (BVal b) = b
beval (Or b1 b2) = beval b1 || beval b2
beval (And b1 b2) = beval b1 && beval b2
beval (Not b) = False

resolve :: String -> Bool
resolve = beval . parseBExpr

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

data LExp
    = OR RExp RExp
    | AND RExp RExp
    | NEG RExp RExp 
    deriving Show
    
    
logical :: Parser LExp
logical = logOr +++ logAnd

logOr :: Parser LExp
logOr = do l1 <- rexp; symbol "|"; l2 <- rexp; return (OR l1 l2)

logAnd :: Parser LExp
logAnd = do l1 <- rexp; symbol "&"; l2 <- rexp; return (AND l1 l2)

parseLExpr :: String -> LExp
parseLExpr rstr = case parse logical rstr of 
    [(n,"")] -> n
    [(_, out)] -> error ("Unused input " ++ out)
    [] -> error "Invalid input"




