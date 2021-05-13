import Control.Monad.State
import System.Random

-- Task 1. count number of letters
type Map a b = [(a,b)]
type Freq a = State (Map a Int) ()

lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup _ [] = Nothing
lookup element (x:xs) | fst x == element = Just (snd x)
                      | otherwise = Main.lookup element xs

update :: Eq a => a -> Map a Int -> Map a Int
update key m = case n of
                    Nothing -> m ++ [(key, 1)]
                    Just n -> [(x, y) | (x, y) <- m, x /= key] ++ [(key, n + 1)]
                where n = Main.lookup key m

freqS :: Eq a => [a] -> State (Map a Int) ()
freqS m = put (foldr update [] m)


-- Ex 2. Monte-Carlo method integration

type R a = State StdGen a

type Range a = (a,a)
type Pair a = (a,a)

randR :: Random a => Range a -> R a
randR r = do g <- get                      -- get the current state, i.e. generator
             let (x, g') = randomR r g     -- generate a random number x together with a new generator g'
             put g'                        -- change the state to g'
             return x                      -- output x

-- randR:: Random a => Range a -> R a
-- randR r = state (randomR r)

randPair :: Random a => Range a -> Range a -> R (Pair a)
randPair xr yr = do
    x <- randR xr        -- generate x-coordinate
    y <- randR yr        -- generate y-coordinate  
    return (x,y)         -- output the generated point


runRandom :: R a -> Int -> a
runRandom action seed = evalState action $ mkStdGen seed

randSeq :: Random a => Range a -> Range a -> Int -> R [Pair a]
randSeq _ _ 0 = return []                        -- if the number of points to be generated is 0, returns []
randSeq xr yr n = do p <- randPair xr yr         -- otherwise, generate a point p
                     ps <- randSeq xr yr (n-1)   -- recursively generate the rest of points ps
                     return (p:ps)               -- output p:ps

integrate :: (Double -> Double) -> Range Double -> Double -> Int -> Double
integrate f xr@(a,b) u n = whole * fromIntegral (length below) / fromIntegral n where  -- compute the area below f
    below = [(x,y) | (x,y) <- samples, y <= f x]                                         -- get the list of points below f
    whole = (b-a)*u                                                                      -- area of the rectangle
    samples = runRandom (replicateM n (randPair xr (0,u))) 123

