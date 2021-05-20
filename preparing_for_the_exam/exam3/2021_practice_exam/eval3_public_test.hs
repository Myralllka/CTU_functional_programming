treeA :: Graph
treeA = [(1, "01", [2]), (2, "01", [1, 3, 4]), (3, "01", [2]), (4, "01", [2, 5]), (5, "01", [4])]
treeB :: Graph
treeB = [(103, "01", [102]), (104, "01", [102, 105]), (101, "01", [102]), (102, "01", [101, 103, 104]), (105, "01", [104]) ]
treeC :: Graph
treeC = [(1000, "01", [1001]), (1001, "01", [1000]) ]

main :: IO ()
main = do putStrLn ""
          putStrLn "treeA :: Graph"
          putStrLn "treeA = [(1, \"01\", [2]), (2, \"01\", [1, 3, 4]), (3, \"01\", [2]), (4, \"01\", [2, 5]), (5, \"01\", [4])]"
          putStrLn "treeB :: Graph"
          putStrLn "treeB = [(103, \"01\", [102]), (104, \"01\", [102, 105]), (101, \"01\", [102]), (102, \"01\", [101, 103, 104]), (105, \"01\", [104]) ]"
          putStrLn "treeC :: Graph"
          putStrLn "treeC = [(1000, \"01\", [1001]), (1001, \"01\", [1000]) ]"
          putStrLn ""
          putStrLn "vertex_certificate 2 treeA"
          putStrLn (vertex_certificate 2 treeA)
          putStrLn ""
          putStrLn "certificate treeA"
          putStrLn (certificate treeA)
          putStrLn "certificate treeB"
          putStrLn (certificate treeB)
          putStrLn "certificate treeC"
          putStrLn (certificate treeC)
          putStrLn ""
          putStrLn "is_isomorphic treeA treeA"
          putStrLn (show (is_isomorphic treeA treeA))
          putStrLn "is_isomorphic treeA treeB"
          putStrLn (show (is_isomorphic treeA treeB))
          putStrLn "is_isomorphic treeA treeC"
          putStrLn (show (is_isomorphic treeA treeC))
          putStrLn ""
          putStrLn "is_isomorphic treeB treeA"
          putStrLn (show (is_isomorphic treeB treeA))
          putStrLn "is_isomorphic treeB treeB"
          putStrLn (show (is_isomorphic treeB treeB))
          putStrLn "is_isomorphic treeB treeC"
          putStrLn (show (is_isomorphic treeB treeC))
          putStrLn ""
          putStrLn "is_isomorphic treeC treeA"
          putStrLn (show (is_isomorphic treeC treeA))
          putStrLn "is_isomorphic treeC treeB"
          putStrLn (show (is_isomorphic treeC treeB))
          putStrLn "is_isomorphic treeC treeC"
          putStrLn (show (is_isomorphic treeC treeC))
          putStrLn ""
