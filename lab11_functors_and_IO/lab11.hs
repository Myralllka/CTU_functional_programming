-- Ex 1. To camel case

toUpper :: Char -> Char 
toUpper c = case lookup c $ zip ['a'..'z'] ['A'..'Z'] of -- $ compose functions
    Nothing -> c -- if there is no result
    Just c' -> c'

toCamelCase :: String -> String 
toCamelCase = concat . map toUpperHead . words where
    toUpperHead "" = ""
    toUpperHead (x:xs) = toUpper x:xs

toCamelCaseF :: Functor f => f String -> f String
toCamelCaseF = fmap toCamelCase

-- Ex 2. Automaton DFA. Check if the input is a number with two digits after the point

data State = Before | Digit | Dot | First | Second | Fail 

data DFA a = Automaton (a -> Char -> a) a (a -> Bool)
isNum :: Char -> Bool
isNum c = c `elem` ['0'..'9']

delta :: State -> Char -> State
delta Before c | isNum c = Digit
               | otherwise = Fail
delta Digit c | isNum c = Digit
              | c == '.' = Dot
              | otherwise = Fail
delta Dot c | isNum c = First
            | otherwise = Fail
delta First c | isNum c = Second
              | otherwise = Fail
delta Second _ = Fail
delta Fail _ = Fail

final :: State -> Bool
final Second = True 
final _ = False 

automaton :: DFA State
automaton = Automaton delta Before final

evalDFA :: DFA a -> String  -> Bool
evalDFA (Automaton dlt s inF) w = inF (foldl dlt s w)

parseNum :: String -> Maybe Float
parseNum w = if evalDFA automaton w then Just (read w)
             else Nothing

parseNumF :: Functor f => f String -> f (Maybe Float)
parseNumF = fmap parseNum
main = print(parseNumF ["24", "123.12", ".5", "0.50"])

-- Ex Monads and >>=

f :: (Eq a, Fractional a) => a -> Maybe a
f x = if x == 0 then Nothing 
                else Just (1/x)

g :: (Ord a, Floating a) => a -> Maybe a
g x = if x > 1 || x < (-1) then Nothing
                           else Just (asin x)

parseIO :: IO()
parseIO = putStrLn "Enter num:"
          >> parseNumF getLine 
          >>= \x -> case x of 
                    Nothing -> parseIO
                    Just _ -> putStrLn "ok"

parseIO2 :: IO()
parseIO2 = do putStrLn "Enter num:"
              x <- parseNumF getLine 
              case x of 
                Nothing -> parseIO
                Just _ -> putStrLn "ok"

-- Task 1

data Expr a = Atom a
            | Neg (Expr a)
            | And (Expr a) (Expr a)
            | Or (Expr a) (Expr a)
                deriving Eq 
                
instance Show a => Show (Expr a) where
    show (Atom c)  = show c
    show (Neg e) = "-" ++ show e
    show (And e1 e2) = "(" ++ show e1 ++ " /\\ " ++ show e2 ++ ")"
    show (Or e1 e2) = "(" ++ show e1 ++ " \\/ " ++ show e2 ++ ")"

fle :: Expr String 
fle = And(Or(Neg (Atom "x"))(Atom "x")) (Atom "y")

-- expr :: Expr Bool -> Bool
-- expr Or (And (Atom True) (Neg (Atom False))) (Atom False)

eval :: Expr Bool -> Bool
eval (Atom c) = c 
eval (Neg e) = not (eval e)
eval (And e1 e2) = eval e1 && eval e2
eval (Or e1 e2) = eval e1 || eval e2

getAtoms::Expr a -> [a]
getAtoms (Atom c) = [c]
getAtoms (Neg e) = getAtoms e 
getAtoms (And e1 e2) = getAtoms e1 ++ getAtoms e2
getAtoms (Or e1 e2) = getAtoms e1 ++ getAtoms e2
