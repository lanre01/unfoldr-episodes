{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module VarArgs where 
import Prelude hiding (sum)
import Data.List



-- Modelling functions that have variable number of arguments
-- without using list

class Sum a where 
    sum' :: Int -> a -- Int is the accumulatd sum 

instance Sum Int where 
    sum' acc = acc  

instance (b ~ Int, Sum a) => Sum (b -> a) where 
    -- sum _= sum @a will always return zero
    sum' :: (b ~ Int, Sum a) => Int -> b -> a
    sum' !acc i = sum' (acc + i)
    -- calling sum' recursively here why?
    -- sum' must return a 
    -- by adding the accumulator to the new Int, we can produce 
    -- another a 

sum :: Sum a => a 
sum = sum' 0 

-- >>> sum (1 :: Int) (2 :: Int) (3 :: Int) :: Int 
-- 6
-- >>> sum 1 2 3 :: Int 
-- 6


class Collect a where 
    collect' :: [Int] -> a 

instance Collect [Int] where 
    collect' xs = xs 

instance (b ~ Int, Collect a) => Collect (b -> a) where 
    collect' :: (b ~ Int, Collect a) => [Int] -> b -> a
    collect' xs x = collect' (x : xs)

collect :: Collect a => a 
collect = collect' []

-- >>> reverse $ collect 1 2 3 :: [Int]
-- [1,2,3]

class VarShow a where 
    varShow' :: [String] -> a 


instance VarShow String where 
    varShow' xs = intercalate "," (reverse xs)

instance (Show b, VarShow a) => VarShow (b -> a) where
    varShow' xs x = varShow' (show x : xs)

varShow :: VarShow a => a 
varShow = varShow' []


-- >>> varShow True False 2 ';' :: String
-- "True,False,2,';'"


data HList xs where 
    HNil :: HList '[]
    HCons :: x -> HList xs -> HList (x : xs)

deriving instance Show (HList '[])
deriving instance (Show x, Show (HList xs)) => Show (HList (x : xs))

class HCollect xs a | a -> xs where 
    hcollect' :: HList xs -> a 

-- Functional dependency 
instance HCollect xs (HList xs) where 
    hcollect' xs = xs 

instance HCollect (x : xs) a => HCollect xs (x -> a) where 
    hcollect' xs x = hcollect' (HCons x xs)


hcollect :: HCollect '[] a => a 
hcollect = hcollect' HNil

isHList :: HList xs -> HList xs 
isHList = id 

-- >>> isHList (hcollect False True 'x' 2)
-- HCons 2 (HCons 'x' (HCons True (HCons False HNil)))
