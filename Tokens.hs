module Tokens where

data Token = S | K | I | A | B | SS | SK | SI | En
  deriving Eq
instance Show Token where
    show S  = "S"
    show K  = "K"
    show I  = "I"
    show A  = "("
    show B  = ")"
    show SS = "s"
    show SK = "k"
    show SI = "i"
    show En = "\n"

tokenise :: String -> [Token]
tokenise [] = []
tokenise (x:xs) | [x] == show A  = A:(tokenise xs)
                | [x] == show B  = B:(tokenise xs)
                | [x] == show S  = S:(tokenise xs)
                | [x] == show K  = K:(tokenise xs)
                | [x] == show I  = I:(tokenise xs)
                | [x] == show SS = SS:(tokenise xs)
                | [x] == show SK = SK:(tokenise xs)
                | [x] == show SI = SI:(tokenise xs)
                | [x] == show En = En:(tokenise xs)
                | otherwise = tokenise xs

--parse :: [Token] -> [Char]
--parse (K:x:y:xs) = (x
--parse (I:x:xs)   = x

