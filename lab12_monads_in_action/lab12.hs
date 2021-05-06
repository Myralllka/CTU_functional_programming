import Data.Char
import Control.Applicative

-- Maze solving, parser

data Block = W | F | S deriving (Eq,Show)

-- W - wall
-- F - free space
-- S - star

data Maze = M [[Block]]

maze :: Maze
maze = M [[W,W,W,W,W],
          [W,F,W,F,W],
          [W,F,W,W,W],
          [W,F,F,F,W],
          [W,W,W,W,W]]


instance Show Maze where
    show (M []) = ""
    show (M (r:rs)) = map dispBlock r ++ "\n" ++ show (M rs)
       where dispBlock W = '#'
             dispBlock F = ' '
             dispBlock S = '*'

type Pos = (Int, Int)
type Path = [Pos]
type Task = (Pos, Pos, Maze)

safePut :: Int -> a -> [a] -> Maybe [a]
safePut n x xs | n `elem` [0..length xs-1] = Just $ take n xs ++ [x] ++ drop (n+1) xs
               | otherwise = Nothing

safeGet :: Int -> [a] -> Maybe a
safeGet n xs | n `elem` [0..length xs-1] = Just $ xs !! n
             | otherwise = Nothing 

getBlock :: Pos -> Maze -> Maybe Block
getBlock (x,y) (M xss) = do row <- safeGet y xss
                            safeGet x row
 
setBlock :: Block -> Pos -> Maze -> Maybe Maze
setBlock b (x,y) (M xss) = do row <- safeGet y xss
                              row <- safePut x b row
                              res <- safePut y row xss
                              return (M res)
                              
setPath :: Maze -> Path -> Maybe Maze
setPath m [] = Just m
setPath m (p:ps) = do m' <- setBlock S p m
                      setPath m' ps

drawSol :: Maze -> Path -> Maze
drawSol m ps = case setPath m ps of
                 Nothing -> m
                 Just m' -> m'

neighbs :: Pos -> [Pos]
neighbs (x,y) = [(x-1,y), (x+1,y), (x,y-1), (x,y+1),
                 (x-1,y-1), (x-1,y+1), (x+1,y-1), (x+1,y+1)]
 
nextPos :: Pos -> Maze -> [Pos]
nextPos p m = case getBlock p m of                                          -- is the input position admissible?
                Just F -> [ p' | p' <- neighbs p, getBlock p' m == Just F]  -- if yes, take all possibilities and filter admissible positions
                _ -> []

extend :: Path -> Maze -> [Path]
extend [] maze = []
extend path maze = map (:path) (nextPos (head path) maze)

solve :: Task -> Maybe Path  
solve (p,q,m) = bfs [] [[p]] q m
 
bfs :: [Pos] -> [Path] -> Pos -> Maze -> Maybe Path
bfs _ [] _ _ = Nothing
bfs visited (path@(p:_):paths) q m                       -- consider the first path in the queue and its head p
    | p == q = Just $ reverse path                       -- is path a solution? If yes, return the reversed solution
    | p `elem` visited = bfs visited paths q m           -- does path end in an already visited position? If yes, disregard it 
    | otherwise = bfs (p:visited) (paths ++ extend path m) q m  -- add p to visited positions and extend path by all possible positions 

main = print (solve ((1,2),(3,3),maze))
