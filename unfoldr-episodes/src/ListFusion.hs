module ListFusion where 

import Prelude hiding (enumFromTo, foldr, map, sum)

{-# INLINE [1] build #-}
build :: (forall r. (a -> r -> r) -> r -> r) -> [a]
build builder = builder (:) []

{-# NOINLINE [1] foldr #-}
foldr :: (a -> r -> r) -> r -> [a] -> r 
foldr cons nil = go 
     where 
        go []       = nil 
        go (x : xs) = x `cons` go xs 


{-# INLINE sum #-}
sum :: [Int] -> Int 
-- sum = go 0
--   where 
--     go !acc [] = acc 
--     go !acc (x : xs) = go (acc + x) xs 
sum list = foldr (\ x f !accum -> f (accum + x)) id list 0  

{-
foldr _ r [] = r
foldr f r (x : xs) = f r (foldr f r xs)


(foldr (\x f acc -> f x + y) id [1,2,3]) 0
(\f acc -> f 1 + y) (\f acc -> f 2 + y)
(\f acc -> f 1 + y) ((\f acc -> f 2 + y) (\f acc -> f 3 + y))
(\f acc -> f 1 + y) ((\f acc -> f 2 + y) ((\f acc -> f 3 + y) id)) 0 
(((\f acc -> f 2 + y) ((\f acc -> f 3 + y) id)) 1) + 0 
(((\f acc -> f 3 + y) id 2) + 1 + 0 
id 3 + 2 + 1 + 0
3 + 2 + 1 + 0


(foldr (\x f acc -> f (x + acc)) id [1,2,3]) 0
(\f acc -> f (1 + acc)) 
(\f acc -> f (1 + acc))  (\f acc -> f (2 + acc))
(\f acc -> f (1 + acc))  ((\f acc -> f (2 + acc)) (\f acc -> f (3 + acc)))
(\f acc -> f (1 + acc))  ((\f acc -> f (2 + acc)) ((\f acc -> f (3 + acc)) id)) 0

(\f acc -> f (2 + acc)) ((\f acc -> f (3 + acc)) id)) (1 + 0)
(\f acc -> f (3 + acc)) id) (2 + 1))
(\f acc -> f (3 + acc) id 3)
id (3 + 3)
id 6 
6 


-}

{-# INLINE enumFromTo #-}
enumFromTo :: Int -> Int -> [Int]
enumFromTo lo hi = 
    -- let 
    --     go i = 
    --         i : 
    --           if i == hi 
    --             then []
    --             else go (i + 1)
    -- in 
    --     if lo > hi then []
    --     else go lo  
    build $ \ cons nil -> 
        let 
            go i = 
                i `cons`
                if i == hi 
                    then nil
                    else go (i + 1)
        in 
            if lo > hi then nil 
            else go lo 
 
{-# INLINE map #-}
map :: (a -> b) -> [a] -> [b] 
-- map _ [] = []
-- map f (x : xs) = f x : map f xs  
map f list = 
    build $ \ cons nil -> 
        foldr (\x r -> f x `cons` r) nil list 


{-# RULES 

    "foldr-build-fusion"  forall cons nil (builder :: forall r. (a -> r -> r) -> r -> r). 
                          foldr cons nil (build builder) = builder cons nil 
#-}