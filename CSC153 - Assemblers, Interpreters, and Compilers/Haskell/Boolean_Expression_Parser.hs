------------------------------------------
-- Ramel Cary B. Jamen

-- <bexp> ::= <bterm> { '|' <bterm> }

-- <bterm> ::= <bfactor> { '&' <bfactor> }

-- <bfactor> ::= '!' <bfactor>
--            | '(' <bexp> ')'
--            | <bterm>
--            | <bval>

-- <bval> ::= 'BVal' ('True' | 'False')

------------------------------------------


import Parsing 

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




