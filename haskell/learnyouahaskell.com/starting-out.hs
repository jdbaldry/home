doubleMe x = x + x
doubleUs x y = x*2 + y*2
doubleSmallNumber x = if x > 100 then x else x*2

safeHead :: [a] -> a
safeHead [] = error "Can't call head on an empty list"
safeHead (x:_) = x

triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10]]
rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2 ]
rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a + b + c == 24]


fizzbuzz :: (Integral a, Eq a, Show a) => a -> String
fizzbuzz num
    | num `mod` 3 == 0 = "fizz"
    | num `mod` 5 == 0 = "buzz"
    | otherwise = show num

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort (filter (>x) xs)
    in smallerSorted ++ [x] ++ biggerSorted

fibs :: Int -> Int
fibs int
    | int == 0 = 1
    | int == 1 = 1
    | otherwise = fibs (int - 1) +  fibs (int - 2)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipwith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x