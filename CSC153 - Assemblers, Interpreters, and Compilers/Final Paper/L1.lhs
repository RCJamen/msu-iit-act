Ramel Cary B. Jamen
2019-2093

Functional parsing library from chapter 8 of Programming in Haskell,
Graham Hutton, Cambridge University Press, 2007.


> import Data.Char
> import Control.Monad
> import qualified Control.Applicative as CA

> infixr 5 +++

> newtype Parser a              =  P (String -> [(a,String)])

> instance CA.Applicative Parser where
>    pure  = return
>    (<*>) = ap

> instance Functor Parser where
>    fmap  = liftM

> instance CA.Alternative Parser where
>    (<|>) = mplus
>    empty = mzero

> instance Monad Parser where
>    return v                   =  P (\inp -> [(v,inp)])
>    p >>= f                    =  P (\inp -> case parse p inp of
>                                                []        -> []
>                                                [(v,out)] -> parse (f v) out)
 
> instance MonadPlus Parser where
>    mzero                      =  P (\inp -> [])
>    p `mplus` q                =  P (\inp -> case parse p inp of
>                                                []        -> parse q inp
>                                                [(v,out)] -> [(v,out)])

> failure                       :: Parser a
> failure                       =  mzero

> item                          :: Parser Char
> item                          =  P (\inp -> case inp of
>                                                []     -> []
>                                                (x:xs) -> [(x,xs)])

> parse                         :: Parser a -> String -> [(a,String)]
> parse (P p) inp               =  p inp

> (+++)                         :: Parser a -> Parser a -> Parser a
> p +++ q                       =  p `mplus` q

> sat                           :: (Char -> Bool) -> Parser Char
> sat p                         =  do x <- item
>                                     if p x then return x else failure

> digit                         :: Parser Char
> digit                         =  sat isDigit
> 
> lower                         :: Parser Char
> lower                         =  sat isLower

> upper                         :: Parser Char
> upper                         =  sat isUpper
> 
> letter                        :: Parser Char
> letter                        =  sat isAlpha

> alphanum                      :: Parser Char
> alphanum                      =  sat isAlphaNum
> 
> char                          :: Char -> Parser Char
> char x                        =  sat (== x)
> 

> string                        :: String -> Parser String
> string []                     =  return []
> string (x:xs)                 =  do char x
>                                     string xs
>                                     return (x:xs)

> many                          :: Parser a -> Parser [a]
> many p                        =  many1 p +++ return []

> many1                         :: Parser a -> Parser [a]
> many1 p                       =  do v  <- p
>                                     vs <- many p
>                                     return (v:vs)

> ident                         :: Parser String
> ident                         =  do x  <- lower
>                                     xs <- many alphanum
>                                     return (x:xs)

> nat                           :: Parser Int
> nat                           =  do xs <- many1 digit
>                                     return (read xs)

> int                           :: Parser Int
> int                           =  do char '-'
>                                     n <- nat
>                                     return (-n)
>                                   +++ nat

> space                         :: Parser ()
> space                         =  do many (sat isSpace)
>                                     return ()

> token                         :: Parser a -> Parser a
> token p                       =  do space
>                                     v <- p
>                                     space
>                                     return v

> identifier                    :: Parser String
> identifier                    =  token ident

> natural                       :: Parser Int
> natural                       =  token nat

> integer                       :: Parser Int
> integer                       =  token int

> symbol                        :: String -> Parser String
> symbol xs                     =  token (string xs)

> p :: Parser (Char, Char)
> p = do 
>       x <- item
>       item
>       y <- item
>       return (x,y) 


Arithmetic Expression Structure

> data AOp
>     = Add | Mul | Sub
>     deriving Show

> data AExp
>     = Num Int
>     | Var String
>     | A AOp AExp AExp
>     deriving Show


Boolean Expression Structure

> data LOp
>     = Or | And
>     deriving Show

> data ROp
>     = Equ | Lt | Gt | Lte | Gte | Ne
>     deriving Show

> data BExp
>     = BVal Bool
>     | Not BExp
>     | R ROp AExp AExp
>     | L LOp BExp BExp
>     deriving Show


Commands Structure 

> data Com
>     = Skip
>     | Print String
>     | Assign String AExp
>     | Seq Com Com
>     | Coms [Com]
>     | If BExp Com Com
>     | WhileDo BExp Com
>     | DoWhile Com BExp
>     | RepUntil Com BExp
>     | ForLoop (Com, BExp, Com) Com
>     deriving Show


Translation and Evaluation Structure

> type Loc = String

> data Ins = PUSH Int | ADD | MUL | SUB
>          | TRUE | FALSE | EQU | GRT | LST | GTE | LTE | AND | OR | NOT
>          | PRINT Loc
>          | LOAD Loc | STORE Loc | NOOP | BRANCH (Code, Code) | LOOP (Code, Code)    
>          deriving Show
         
> type Code = [Ins] 


Abstract Machine State Structure

> data ValZB      = Z Int | B Bool deriving Show
> type Stack      = [ValZB]
> type Binding    = (Loc, Int)
> type Storage    = [Binding]
> type Debug      = [Binding]
> type MState     = (Code, Stack, Storage, Debug)


Parsing Arithmitic Expression

> expr :: Parser AExp
> expr = do t <- term
>           do symbol "-"; e <- expr; return (A Sub t e)
>             +++ do symbol "+"; e <- expr; return (A Add t e)
>              +++ return t

> term :: Parser AExp
> term = do f <- factor
>           do symbol "*"; t <- term; return (A Mul f t)
>              +++ return f

> factor :: Parser AExp
> factor = do symbol "("
>             e <- expr; symbol ")"; return e
>             +++ number
>             +++ var

> number :: Parser AExp
> number = do x <- natural; return (Num x)

> var :: Parser AExp
> var = do v <- identifier; return (Var v)


Parsing Boolean Expression

> bexp :: Parser BExp
> bexp = (do t <- bterm; symbol "|"; e <- bexp;  return (L Or t e)) +++ bterm

> bterm :: Parser BExp
> bterm = (do f <- bfactor; symbol "&"; t <- bterm; return (L And f t)) +++ bfactor

> bfactor :: Parser BExp
> bfactor = nbval +++ bval 

> bval :: Parser BExp
> bval = bpar +++ tval +++ rexp

> nbval :: Parser BExp
> nbval = do symbol "!"; f <- bfactor; return (Not f)

> tval :: Parser BExp
> tval = (do symbol "F"; return (BVal False)) +++ (do symbol "T"; return (BVal True))

> bpar :: Parser BExp
> bpar = do symbol "("; e <- bexp; symbol ")"; return e

> rexp :: Parser BExp
> rexp = eq +++ lt +++ gt +++ lteq +++ gteq +++ noteq


Parsing Relational Expression

> eq :: Parser BExp
> eq = do l1 <- expr; symbol "=="; l2 <- expr; return (R Equ l1 l2)

> lt :: Parser BExp
> lt = do l1 <- expr; symbol "<"; l2 <- expr; return (R Lt l1 l2)

> gt :: Parser BExp
> gt = do l1 <- expr; symbol ">"; l2 <- expr; return (R Gt l1 l2)

> lteq :: Parser BExp
> lteq = do l1 <- expr; symbol "<="; l2 <- expr; return (R Lte l1 l2)

> gteq :: Parser BExp
> gteq = do l1 <- expr; symbol ">="; l2 <- expr; return (R Gte l1 l2)

> noteq :: Parser BExp
> noteq = do l1 <- expr; symbol "!="; l2 <- expr; return (R Ne l1 l2)


Parsing Commands and Statements

> coms = do lst <- (sep0 comparse (symbol ";")); return (if length lst == 1 then head lst else Coms lst) 

> seqcom :: Parser Com
> seqcom = do t <- comparse; symbol ";"; e <- seqcom; return (Seq t e)
>             +++ comparse
    
> comparse :: Parser Com
> comparse = skip +++ block +++ cblock +++ assign +++ printcom +++ ifchoice +++ wdloop +++ dwloop +++ repuntil +++ forloop   

> skip :: Parser Com
> skip = do symbol "skip"; return Skip

> assign :: Parser Com
> assign = do id <- identifier; symbol ":="; e <- expr; return (Assign id e)

> printcom :: Parser Com
> printcom = do symbol "print"; symbol "("; v <- identifier; symbol ")"; return (Print v)

> block :: Parser Com
> block = do symbol "begin"; t <- coms; symbol "end"; return (t) +++ comparse

> cblock :: Parser Com
> cblock = do symbol "{"; t <- coms; symbol "}"; return (t) +++ comparse

> ifchoice :: Parser Com
> ifchoice = do symbol "if"; b <- bexp; symbol "then"; ct <- comparse; symbol "else"; cf <- comparse; return (If b ct cf)

> wdloop :: Parser Com
> wdloop = do symbol "while"; b <- bexp; symbol "do"; ct <- comparse; return (WhileDo b ct)

> dwloop :: Parser Com
> dwloop = do symbol "do"; b <- comparse; symbol "while"; ct <- bexp; return (DoWhile b ct)

> repuntil :: Parser Com
> repuntil = do symbol "repeat"; b <- comparse; symbol "until"; ct <- bexp; return (RepUntil b ct)

> forloop :: Parser Com
> forloop = do symbol "for"; symbol "("; b <- assign; symbol ";"; ct <- bexp; symbol ";"; cf <- assign; symbol ")"; symbol "do"; c <- comparse; return (ForLoop (b, ct, cf) c)

    
Translate an Arithmetic Expression to Abstract Machine Code

> tr_a :: AExp -> Code
> tr_a (Num n)        = [PUSH n]
> tr_a (Var x)        = [LOAD x]
> tr_a (A Add a1 a2)  = tr_a a2 ++ tr_a a1 ++ [ADD]
> tr_a (A Sub a1 a2)  = tr_a a2 ++ tr_a a1 ++ [SUB]
> tr_a (A Mul a1 a2)  = tr_a a2 ++ tr_a a1 ++ [MUL]

Translate a Boolean Expression to Abstract Machine Code

> tr_b :: BExp -> Code
> tr_b (BVal True)    = [TRUE]
> tr_b (BVal False)   = [FALSE]
> tr_b (Not l1)       = tr_b l1 ++ [NOT]
> tr_b (R Equ l1 l2)  = tr_a l2 ++ tr_a l1 ++ [EQU]
> tr_b (R Gt l1 l2)   = tr_a l2 ++ tr_a l1 ++ [GRT]
> tr_b (R Lt l1 l2)   = tr_a l2 ++ tr_a l1 ++ [LST]
> tr_b (R Gte l1 l2)  = tr_a l2 ++ tr_a l1 ++ [GTE]
> tr_b (R Lte l1 l2)  = tr_a l2 ++ tr_a l1 ++ [LTE]
> tr_b (L And l1 l2)  = tr_b l1 ++ tr_b l2 ++ [AND]
> tr_b (L Or l1 l2)   = tr_b l1 ++ tr_b l2 ++ [OR]

Translate commands to abstract machine code

> tr_c :: Com -> Code
> tr_c (Coms (c:cs))  =  tr_c c ++ if length cs == 1 then tr_c (head cs) else tr_c (Coms cs)
> tr_c (Skip)         = [NOOP]
> tr_c (Print v)      = [PRINT v]
> tr_c (Assign m v)   = tr_a v ++ [STORE m]
> tr_c (If b c1 c2)   = tr_b b ++ [BRANCH(tr_c c1, tr_c c2)]
> tr_c (WhileDo b c)  = [LOOP (tr_b b, tr_c c)]
> tr_c (DoWhile c b)  = tr_c (Coms (c : [WhileDo b c]))
> tr_c (RepUntil c b) = tr_c (Coms (c : [WhileDo (Not b) c]))
> tr_c (ForLoop (c1, b, c2) c3) = tr_c (Coms (c1 : [WhileDo b (Coms (c3 : [c2]))]))

Evaluate Abstract Machine Code (Single Step)

> amEval :: MState -> MState
> amEval (PUSH n:cs, e, s, d)            = (cs, Z n:e, s, d)
> amEval (ADD:cs, Z n1: Z n2:e, s, d)    = (cs, Z (n1 + n2):e, s, d)
> amEval (SUB:cs, Z n1: Z n2:e, s, d)    = (cs, Z (n1 - n2):e, s, d)
> amEval (MUL:cs, Z n1: Z n2:e, s, d)    = (cs, Z (n1 * n2):e, s, d)
> amEval (TRUE:cs, e, s, d)              = (cs, B True:e, s, d)
> amEval (FALSE:cs, e, s, d)             = (cs, B False:e, s, d)
> amEval (EQU:cs, Z n1: Z n2:e, s, d)    = (cs, B (n1 == n2):e, s, d)
> amEval (GRT:cs, Z n1: Z n2:e, s, d)    = (cs, B (n1 > n2):e, s, d)
> amEval (LST:cs, Z n1: Z n2:e, s, d)    = (cs, B (n1 < n2):e, s, d)
> amEval (LTE:cs, Z n1: Z n2:e, s, d)    = (cs, B (n1 <= n2):e, s, d)
> amEval (GTE:cs, Z n1: Z n2:e, s, d)    = (cs, B (n1 >= n2):e, s, d)
> amEval (AND:cs, B b1: B b2:e, s, d)    = (cs, B (b1 && b2):e, s, d)
> amEval (OR:cs, B b1: B b2:e, s, d)     = (cs, B (b1 || b2):e, s, d)
> amEval (NOT:cs, B b:e, s, d)           = (cs, B (not b):e, s, d)
> amEval (LOAD x:cs, e, s, d)            = (cs, Z (valueOf x s):e, s, d) 
> amEval (STORE x:cs, Z z:e, s, d)       = (cs, e, update s x z, d) 
> amEval (NOOP:cs, e, s, d)              = (cs, e, s, d) 
> amEval (BRANCH (c1, c2):cs, B b:e, s, d)  = if b then (c1 ++ cs, e, s, d) else (c2 ++ cs, e, s, d)
> amEval (LOOP(c1, c2):cs, b, s, d)      = (c1 ++ BRANCH(c2 ++ [LOOP(c1, c2)],[NOOP]):cs, b, s, d)
> amEval (PRINT x:cs, e, s, d)           = (cs, e, s, (x,valueOf x s):d)
> amEval (c:cs, e, s, d)                 = (cs, e, s, d)
> amEval ([], e, s, d)                   = ([], e, s, d)


Utility Functions for Commands Separator

> sep1 :: Parser Com -> Parser a -> Parser [Com]
> sep1 p sep = do x <- p; xs <- many (sep >> p); return (x : xs)

> sep0 :: Parser Com -> Parser sep -> Parser [Com]
> sep0 p sep = sep1 p sep +++ return []


Utility Functions for Storage and Debug

> valueOf :: Loc -> Storage -> Int
> valueOf loc [] = error ("Unused input " ++ loc)
> valueOf loc ((key, val):rest)
>     | loc == key = val
>     | otherwise = valueOf loc rest
        
> update :: Storage -> Loc -> Int -> Storage
> update [] loc newVal = [(loc, newVal)]
> update ((key, val) : rest) loc newVal
>     | loc == key = (loc, newVal) : rest
>     | otherwise = (key, val) : update rest loc newVal


Interface Functions

> parseAExpr :: String -> AExp
> parseAExpr xs = case (parse expr xs) of
>     [(n,[])] -> n
>     [(_,out)] -> error ("Unused input " ++ out)
>     [] -> error "Invalid input"

> parseBExpr :: String -> BExp
> parseBExpr rstr = case parse bexp rstr of
>     [(n,"")] -> n
>     [(_, out)] -> error ("Unused input " ++ out)
>     [] -> error "Invalid input"

> parse_com :: String -> Com
> parse_com rstr = case parse seqcom rstr of
>     [(n,"")] -> n
>     [(_, out)] -> error ("Unused input " ++ out)
>     [] -> error "Invalid input"
    
> parse_coms :: String -> Com
> parse_coms rstr = case parse coms rstr of
>     [(n,"")] -> n
>     [(_, out)] -> error ("Unused input " ++ out)
>     [] -> error "Invalid input"

> compile :: String -> Code
> compile = tr_c . parse_coms

> amInit :: Code -> MState
> amInit xs = (xs, [], [], [])

> eval :: MState -> MState
> eval ([], e, s, d) = ([], e, s, d)
> eval state = eval (amEval state)

> amExec :: String -> MState
> amExec prog = eval (amInit (compile prog))

> amGetVal :: String -> String -> Int
> amGetVal var prog = result where 
>         result  = valueOf var storage
>         (_,_,storage,_)   = eval (amInit (compile prog))

> amRun :: String -> IO ()
> amRun prog = mapM_ (putStrLn . show . snd) (debugList)
>     where
>         (_, _, _, debug) = eval (amInit (compile prog))
>         debugList = reverse debug
