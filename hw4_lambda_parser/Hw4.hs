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
var = do name <- sat isAlphaNum
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

def :: Parser Glob
def = token definition

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

checkReinit :: [Char] -> [Char] -> Bool
checkReinit _ "" = False
checkReinit var_name str | (var_name ++ " :=") `isInfixOf` str = True
                         | otherwise = False

replaceDefinitions :: String -> String -> String -> String
replaceDefinitions _ _ "" = ""
replaceDefinitions source target (x:xs) | checkReinit source (x:xs) = x:xs
                                        | otherwise = replace source target (x:xs)


definitions :: String -> Maybe String
definitions str | ":=" `isInfixOf` str =  case parse def str of
                                        Nothing -> Nothing
                                        Just (Gl a b, s) -> definitions (replaceDefinitions a (show b) s)
                | otherwise = Just str


readPrg :: String -> Maybe Expr
readPrg str = fst <$> parse expr (new_str)
                        where new_str = case definitions str of
                                            Nothing -> ""
                                            Just x -> x

-- readPrg str = do new_str <- definitions str 
                --  return fst <$> parse expr new_str

-- main :: IO ()
-- main = do inp <- getContents
--           case readPrg inp of
--                 Nothing -> putStrLn "Incorrect program"
--                 Just e -> print $ e