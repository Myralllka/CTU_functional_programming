-- Lecture 12
import Control.Applicative
import Data.Char
import Data.List
import Parser

-- Example Maybe as applicative functor

validateLength :: Int -> String -> Maybe String
validateLength maxLen s = if (length s) > maxLen
                          then Nothing
                          else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 12 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 25 a

data Person = Person Name Address deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a = pure Person <*> mkName n <*> mkAddress a
-- It can be done also via monadic instance of Maybe. 
{-
mkPerson n a = do name <- mkName n
                  addr <- mkAddress a
                  return $ Person name addr
-}

-- Monadic parsing

data Expr a = Val a 
            | Var String 
            | Add [Expr a] 
            | Mul [Expr a] deriving Eq

instance Show a => Show (Expr a) where
    show (Val c) = show c
    show (Var s) = s
    show (Add es) = "(" ++ intercalate " + " (map show es) ++ ")"
    show (Mul es) = "(" ++ intercalate " * " (map show es) ++ ")"

{-
<expr> -> <space>* <expr'> <space>*
<expr'> -> <var>
         | <val>
         | <add>
         | <mul>

<var> -> <lower> <alphanum>+
<val> -> <int> "." <digit>+ | <int>
<int> -> "-" <digit>+ | <digit>+ 

<add> -> "(" <expr> ("+" <expr>)+ ")"
<mul> -> "(" <expr> ("*" <expr>)+ ")"

-}

var :: Parser (Expr a)
var = do x <- sat isLower 
         xs <- many alphaNum
         return $ Var (x:xs)

digit :: Parser Char
digit = sat isDigit 

nat :: Parser String
nat = some digit

int :: Parser String
int = do char '-'
         xs <- nat
         return ('-':xs)
      <|> nat

float :: Parser Float
float = do xs <- int
           char '.'
           ys <- nat
           return $ read (xs ++ "." ++ ys)
        <|> read <$> int 

space :: Parser ()
space = many (sat isSpace) >> return ()

token :: Parser a -> Parser a
token p = do space
             x <- p
             space
             return x

val :: Parser (Expr Float)
val = Val <$> float

expr :: Parser (Expr Float)
expr = token (var <|> val <|> op '+' <|> op '*')      

opCons :: Char -> [Expr a] -> Expr a
opCons '+' = Add
opCons '*' = Mul
opCons c = error $ show c ++ " is unknown op"

op :: Char -> Parser (Expr Float)
op c = do char '('
          e <- expr
          es <- some (char c >> expr >>= \e' -> return e')
          char ')'
          return $ opCons c (e:es)