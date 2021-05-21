module Hw4 where

import Control.Applicative
import Data.Char
import Data.List
import Parser
import Hw3

-- Parser contains predefined parsers for:
-- sep - separator
-- string - seq of letters
-- isSpace - check if is space
-- isAlphaNum - check if is valid varname


-- <program> -> (<definition> <sep>)* <expr> <space>*
-- <definition> -> <var> <sep> ":=" <sep> <expr>
-- @ <expr> -> <var>
-- @ | "(\\" <var> '.' <expr> ')'
-- @ | '(' <expr> <sep> <expr> ')'
-- @ <var> -> <alphanum>+
-- @ <alphanum> -> any alphanumeric character recognized by isAlphaNum
-- @ <sep> -> <space>+
-- @ <space> -> any whitespace recognized by isSpace, e.g. ' ', '\n'

data Glob = Gl String Expr

instance Show Glob where
    show (Gl n e) = "||" ++ show n ++ " := " ++ show e ++ "||"

variables :: [Glob]
variables = []

space :: Parser ()
space = many (sat isSpace) >> return ()

expr :: Parser Expr
expr = token (lambda <|> app <|> var)

token :: Parser a -> Parser a
token p = do space
             x <- p
             space
             return x

var :: Parser Expr
var = do name <- sat isAlpha
         xs <- many alphaNum

         return (Var (name:xs))

lambda :: Parser Expr
lambda = do char '('
            char '\\'
            v <- some alphaNum
            char '.'
            exp <- expr >>= \e' -> return e'
            char ')'
            return $ Lambda v exp

app :: Parser Expr
app = do char '('
         ex1 <- expr
         space
         ex2 <- expr >>= \e' -> return e'
         char ')'
         return (App ex1 ex2)


definition :: Parser Glob
definition = do name <- sat isAlphaNum
                names <- many alphaNum
                token (string ":=")
                exp <- expr >>= \e' -> return e'
                return (Gl (name:names) exp)

replace :: [Char] -> [Char] -> [Char] -> [Char]
replace _ _ "" = ""
replace source target (x:xs) | source `isPrefixOf` (x:xs) = target ++ replace source target [(x:xs) !! i | i <- [start..stop]]
                             | otherwise = x : replace source target xs
                                    where start = length source
                                          stop = length xs

-- 0 := (\\s.(\\z.z)) \n S := (\\w.(\\y.(\\x.(y ((w y) x)))))

definitions :: String -> String
definitions str = case parse definition str of
                 Nothing -> str
                 Just (Gl a b, s) -> definitions (replace a (show b) s)

readPrg :: String -> Maybe Expr
readPrg str = fst <$> parse expr (definitions str)
                
-- main :: IO ()
-- main = do inp <- getContents
--           case readPrg inp of
--                 Nothing -> putStrLn "Incorrect program"
--                 Just e -> print $ e
                
-- variables = case fst <$> parse definition "a := 1" of
--                 Nothing -> Gl "" (Var "x")
--                 Just x -> x


-- main = parse definition