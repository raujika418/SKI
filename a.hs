type Cont a = (Maybe Char, Int) -> a -> IO Exp

data Exp
    = App Exp Exp
    | K
    | K1 Exp
    | S
    | S1 Exp
    | S2 Exp Exp
    | I
    | V
    | C
    | Cont (Cont Exp)
    | D
    | D1 Exp
    | Dot Char
    | E
    | At
    | Ques Char
    | Pipe

instance Show Exp where
  showsPrec _ = sh

sh :: Exp -> String -> String
sh (App x y)  = showChar '`' . sh x . sh y
sh K          = showChar 'k' 
sh (K1 x)     = showString "`k" . sh x
sh S          = showChar 's' 
sh (S1 x)     = showString "`s" . sh x
sh (S2 x y)   = showString "``s" . sh x . sh y
sh I          = showChar 'i' 
sh V          = showChar 'v' 
sh C          = showChar 'c' 
sh (Cont _)   = showString "<cont>"
sh D          = showChar 'd' 
sh (D1 x)     = showString "`d" . sh x
sh (Dot '\n') = showChar 'r' 
sh (Dot c)    = showChar '.' . showChar c
sh E          = showChar 'e' 
sh At         = showChar '@'
sh (Ques c)   = showChar '?' . showChar c
sh Pipe       = showChar '|'
