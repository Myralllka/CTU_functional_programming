import Data.List

type Node = (Int, String, [Int])
type Graph = [Node]

initLabel :: Graph -> Graph
initLabel [] = []
initLabel ((v, _, vs):ns) = (v, "01", vs):(initLabel ns)

isLeaf :: Graph -> Int -> Bool
isLeaf [] _ = False
isLeaf ((n, _, vs):ns) i | n == i && length vs == 1 = True
                         | otherwise = isLeaf ns i 

extractLabel :: Graph -> Int -> String
extractLabel [] _ = ""
extractLabel ((n, l, _):ns) i | n == i = l
                              | otherwise = extractLabel ns i

newLabel :: Graph -> Node -> String
newLabel g (n, l, vs) = add01 . join . sort $ (trimm l):[extractLabel g v | v <- vs, isLeaf g v]

trimm :: String -> String
trimm s = [s !! i | i <- [1..length s-2]]

join :: [String] -> String
join [] = ""
join (x:xs) = x ++ (join xs)

add01 :: String -> String
add01 s = "0" ++ s ++ "1"

iter :: Graph -> Graph
iter ns = [(v, newLabel ns n, updateEdges ns vs) | n@(v, l, vs) <- ns, not (isLeaf ns v)]
   
updateEdges :: Graph -> [Int] -> [Int]
updateEdges ns vs = [v | v <- vs, not (v `elem` leafs)] where
    leafs = [v | (v,_,_) <- ns, isLeaf ns v]

cert :: Graph -> String
cert [] = ""
cert [(_, l, _)] = l
cert [(_, l1, _), (_, l2, _)] = join . sort $ [l1, l2]
cert g = cert (iter g)

extract_node :: Graph -> Int -> Node
extract_node (n@(nid,_,_):gs) id | nid == id =  n
                                 | otherwise = extract_node gs id


vertex_certificate :: Int -> Graph -> String
vertex_certificate id g = newLabel g (extract_node g id)

certificate :: Graph -> String
certificate g = cert g

is_isomorphic :: Graph -> Graph -> Bool
is_isomorphic g1 g2 = ( certificate g1 == certificate g2 )
