(define treeA '( (1 "01" (2)) (2 "01" (1 3 4)) (3 "01" (2)) (4 "01" (2 5)) (5 "01" (4)) ))
(define treeB '( (103 "01" (102)) (104 "01" (102 105)) (101 "01" (102)) (102 "01" (101 103 104)) (105 "01" (104)) ))
(define treeC '( (1000 "01" (1001)) (1001 "01" (1000)) ))


(display "(define treeA '( (1 \"01\" (2)) (2 \"01\" (1 3 4)) (3 \"01\" (2)) (4 \"01\" (2 5)) (5 \"01\" (4)) ))")(display "\n")
(display "(define treeB '( (103 \"01\" (102)) (104 \"01\" (102 105)) (101 \"01\" (102)) (102 \"01\" (101 103 104)) (105 \"01\" (104)) ))")(display "\n")
(display "(define treeC '( (1000 \"01\" (1001)) (1001 \"01\" (1000)) ))")(display "\n")
(display "\n")

(display "(vertex-certificate 2 treeA)\n")(display (vertex-certificate 2 treeA))(display "\n")
(display "\n")

(display "(certificate treeA)\n")(display (certificate treeA))(display "\n")
(display "(certificate treeB)\n")(display (certificate treeB))(display "\n")
(display "(certificate treeC)\n")(display (certificate treeC))(display "\n")
(display "\n")

(display "(display (isomorphic? treeA treeA))\n")(display (isomorphic? treeA treeA))(display "\n")
(display "(display (isomorphic? treeA treeB))\n")(display (isomorphic? treeA treeB))(display "\n")
(display "(display (isomorphic? treeA treeC))\n")(display (isomorphic? treeA treeC))(display "\n")
(display "\n")
(display "(display (isomorphic? treeB treeA))\n")(display (isomorphic? treeB treeA))(display "\n")
(display "(display (isomorphic? treeB treeB))\n")(display (isomorphic? treeB treeB))(display "\n")
(display "(display (isomorphic? treeB treeC))\n")(display (isomorphic? treeB treeC))(display "\n")
(display "\n")
(display "(display (isomorphic? treeC treeA))\n")(display (isomorphic? treeC treeA))(display "\n")
(display "(display (isomorphic? treeC treeB))\n")(display (isomorphic? treeC treeB))(display "\n")
(display "(display (isomorphic? treeC treeC))\n")(display (isomorphic? treeC treeC))(display "\n")
(display "\n")

