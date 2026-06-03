{- HLINT ignore "Use lambda-case" -}
module Unfoldr where


-- My understanding 
-- unfoldr is just treating a list as a coindcutive data type
-- with coiduction, we can work on infinite data types
-- so is foldl also coinductive? so the unfoldl will also be inductive?

{-
foldr :: (a -> b -> b) -> b -> [a] -> b 
foldr _ b [] = b 
foldr f b (a:as) = f a (foldr f b as)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ b [] = b 
foldl f b (a:as) = foldl f (f b a) as 

-}

unfoldr :: (s -> Maybe (a, s)) -> s -> [a]
unfoldr gen s =
    case gen s of
        Nothing -> []
        Just (a, s') -> a : unfoldr gen s'

unfoldl :: (s -> Maybe(s, a)) -> s -> [a]
unfoldl gen s =
    case gen s of
        Nothing -> []
        Just (s', a) -> unfoldl gen s' ++ [a]

-- examples

enumFromTo' :: Int -> Int -> [Int]
enumFromTo' lo hi =
    unfoldr
      (\cur -> if cur > hi then Nothing else  Just (cur, cur + 1))
      lo

enumFromTo'' :: Int -> Int -> [Int]
enumFromTo'' lo = 
    unfoldl
      (\cur -> if cur < lo then Nothing else  Just (cur - 1, cur))
      
fibs :: [Int]
fibs =
    unfoldr
      (\(l, r) -> Just (l, (r, l + r)))
      (0, 1)

map' :: (a -> b) -> [a] -> [b]
map' f = unfoldr (\x ->
                     case x of
                        [] -> Nothing
                        (a:as) -> Just (f a, as)
                 )

zip' :: [a] -> [b] -> [(a, b)]
zip' as bs =
    unfoldr
      (\s ->
        case s of
            (x:xs, y:ys) -> Just ((x,y), (xs, ys))
            _            -> Nothing
      )
      (as, bs)



