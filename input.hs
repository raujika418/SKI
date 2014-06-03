import qualified Text.Parsec as P
import Control.Applicative ((*>), (<$>), (<*>))

data AST = Apply AST AST | Val Value
instance Show AST where
  show (Apply a b) = "(" ++ show a ++ " " ++ show b ++ ")"
  show (Val (Dot c)) = "put-" ++ [c]
  show (Val (Builtin c)) = [c]

data Value = Dot Char
  | Builtin Char
  | PendingK Value
  | PendingS1 Value
  | PendingS2 Value Value
  deriving Show

main = do
  let helloworld = "`r```````````.H.e.l.l.o. .w.o.r.l.di"
  let fibonacci = "```s``s``sii`ki`k.*``s``s`ks``s`k`s`ks``s``s`ks``s`k`s`kr``s`k`sikk`k``s`ksk"
  print $ desugar $ parse helloworld
  eval $ desugar $ parse helloworld
  --eval $ desugar $ parse fibonacci

parse :: String -> AST
parse = either (error . show) id . P.parse parse' "unlambda"

parse' = P.try (P.char '`' *> (Apply <$> parse' <*> parse'))
         P.<|>
         P.try (P.char '.' *> (Val . Dot <$> P.anyChar))
         P.<|>
         P.try (Val . Builtin <$> P.anyChar)

desugar :: AST -> AST
desugar (Apply a b) = Apply (desugar a) (desugar b)
desugar (Val (Builtin 'r')) = Val (Dot '\n')
desugar (Val (Builtin 'i')) = Apply (Apply (Val (Builtin 's')) (Val (Builtin 'k'))) (Val (Builtin 'k')) -- i = ``skk
desugar x = x

eval :: AST -> IO (Value)
eval (Apply a b) = do
  a' <- eval a
  b' <- eval b
  apply a' b'
eval (Val x) = return x

apply :: Value -> Value -> IO Value
apply (Dot c) x = putChar c >> return x
apply (Builtin 'k') x = return $ PendingK x
apply (Builtin 's') x = return $ PendingS1 x
apply (PendingK x) y = return $ x
apply (PendingS1 x) y = return $ PendingS2 x y
apply (PendingS2 x y) z = do
  a <- apply x z
  b <- apply y z
  apply a b
