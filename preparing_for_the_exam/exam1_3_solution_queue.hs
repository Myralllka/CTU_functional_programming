-- Same task, but on Haskell

type Stack v = [v]


push :: v -> Stack v -> Stack v
push value stack = value:stack


pop :: Stack v -> Stack v
pop []           = []
pop (_:newStack) = newStack

peek :: Stack v -> Maybe v
peek q | null q = Nothing 
       | otherwise = Just (head q)


data Queue v = InStackOutStack (Stack v) (Stack v) deriving (Eq, Show)

enqueue :: v -> Queue v -> Queue v
enqueue el (InStackOutStack s1 s2) = InStackOutStack (push el s1) s2

dispatch :: Queue v -> Queue v
dispatch (InStackOutStack s1 s2) | null s1 = InStackOutStack s1 s2
                                 | otherwise = case peek s1 of
                                                Nothing -> dispatch (InStackOutStack s1 s2)
                                                Just a -> dispatch (InStackOutStack (pop s1) (push a s2))

dequeue :: Queue v -> Queue v
dequeue (InStackOutStack s1 s2) | null s1 && null s2 = q
                                | null s2 = dequeue (dispatch q)
                                | otherwise = InStackOutStack s1 (pop s2)
                            where q = InStackOutStack s1 s2
-- Testing


emptyStack = []

stack0 = push 0 emptyStack

stack01 = push 1 stack0

completeStack = push 7 . push 6 . push 5 . push 4 . push 3 . push 2 . push 1 . push 0 $ emptyStack

stack42 = push 7 . push 6 . push 5 . push 42 . pop . pop . pop . pop $ completeStack

inStack  = push 7 . push 6 . push 5 . push 4 $ emptyStack

outStack = push 3 . push 2 . push 1 . push 0 $ emptyStack

listOfStacks = [emptyStack, stack0, stack01, completeStack, stack42, inStack, outStack]

q1 = InStackOutStack inStack outStack
q2 = InStackOutStack emptyStack outStack
q3 = InStackOutStack inStack emptyStack

listOfQueues = [q1, q2, q3]
