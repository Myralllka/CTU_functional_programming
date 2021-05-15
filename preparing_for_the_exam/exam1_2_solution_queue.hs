-- Same task, but on Haskell

type Stack v = [v]


push :: v -> Stack v -> Stack v
push value stack = value:stack


pop :: Stack v -> Stack v
pop []           = []
pop (_:newStack) = newStack


data Queue v = InStackOutStack (Stack v) (Stack v) deriving (Eq, Show)

enqueue :: v -> Queue v -> Queue v
enqueue el q = 

dispatch :: Queue v -> Queue v

dequeue :: Queue v -> Queue v
