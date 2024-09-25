-- Ramel Cary B. Jamen
-- 2019-2093

import Parsing
import Expressions

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
seqcom = do c <- comparse; symbol ";"; do cc <- seqcom; return (Seq c cc)    
            +++ comparse

comparse :: Parser Com
comparse = skip +++ assign +++ choice +++ loop
    
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
