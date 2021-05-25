type ZipList a = ([a], [a])

-- Creates an empty list
newList :: ZipList a
newList = ([], [])

-- Moves the focus to the left. If this is not possible the program may crash.
moveLeft :: ZipList a -> ZipList a
moveLeft (x:xs, y:ys) = (xs, x:y:ys)
moveLeft (x:xs, []) = (xs, [x])

-- -- Moves the focus to the right. If this is not possible the program may crash.
moveRight :: ZipList a -> ZipList a
moveRight (x:xs, y:ys) = (y:x:xs, ys)
-- moveRight (x:xs, y) = (head y:x:xs, [])
moveRight ([], y:ys) = ([y], ys)

-- -- Rewinds a zipper list so that the focus is on the beginning of the list.
rewind :: ZipList a -> ZipList a
rewind ([], a) = ([], a)
rewind (x:xs, ys) = rewind (xs, x:ys)

-- -- Returns the index the zipper list is foccused on.
index :: ZipList a -> Int
index (xs, ys) = length xs

-- -- moveTo n zl moves the focus of zl to the n-th index of the list. If n is bigger than the size of the list the program may crash.
moveTo :: Int -> ZipList a -> ZipList a
moveTo n (xs, ys) | n < length xs = moveTo n (moveLeft (xs, ys))
                  | n == length xs = (xs, ys)
                  | otherwise = moveTo n (moveRight (xs, ys))

-- -- Returns the element of the list that is currently to the right of the focus. If there is no element to the right of the focus the program may crash.
get :: ZipList a -> a
get (_, y:ys) = y

-- -- Updates the element to the right of the focus. If there is no element to the right of the focus, the program may crash.
update :: a -> ZipList a -> ZipList a
update el (xs, y:ys) = (xs, el:ys)

-- -- Insert a new element to the right of the focus.
insert :: a -> ZipList a -> ZipList a
insert el (xs, ys) = (xs, el:ys)

-- -- Deletes the element to the right of the focus. If there is no element to the right, the program may crash.
delete :: ZipList a -> ZipList a
delete (xs, y:ys) = (xs, ys)

t1 = insert 1 . insert 2 . insert 3 . insert 4 . insert 5 $ newList
-- ([],[1,2,3,4,5])

t2 = delete .  moveRight . moveRight . moveRight . moveRight
   . insert 102 . moveRight
   . update 101 . moveLeft . moveLeft . moveLeft
   . update 100 . moveRight . moveRight . moveRight $ t1
-- ([100,3,2,102,101],[])

t3 = moveLeft . moveLeft $ t2
-- ([2,102,101],[3,100])

t4 = get t3
-- 3

t5 = rewind t2
-- ([],[101,102,2,3,100])

t6 = index t3
-- 3

t7 = moveTo 1 t3
-- ([101],[102,2,3,100])

