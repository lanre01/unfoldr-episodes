{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module ComputingConstraints where 

import Data.Functor.Identity 
import Data.Kind 


-- Given this definition of hetorgenous list, can we define a Show
-- instance using deriving?
type HList :: [Type] -> Type 
data HList xs where 
    HNil :: HList '[]
    (:-) :: x -> HList xs -> HList (x : xs)
    -- deriving Show
    -- This fails because ghc can not instanciate a show instance for this 
    -- type
infixr 5 :- 

-- One way to derive an instance of Show is to do it inductively
-- This works but this only tells use that the heterogenous list is 
-- showable. But what if we want to pattern match on the different 
-- elements in the data type like in the comma function?
deriving instance Show (HList '[])
deriving instance (Show x, Show (HList xs)) => Show (HList (x : xs))


example1 :: HList [Bool, Int, Char]
example1 = True :- 1 :- 'f' :- HNil

-- >>> show example1
-- "True :- (1 :- ('f' :- HNil))"



-- commas :: Show xs => HList xs -> String 
-- Why does this fail?
-- By examining what the deriving clause does for HList, it can observed
-- that the show instance pertains to (HList xs) and not xs 
-- so technically we don't know if xs is showable. We know the elements
-- of xs are showable but xs as whole is not necessarily showable.
-- So, this Showable instance is on (HList xs) 
-- But in essence to enfore Show xs one must enforce show on each element of xs 
-- This is what the All class enforces.
commas :: All Show xs => HList xs -> String 
commas HNil        = ""
commas (x :- HNil) = show x
commas (x :- xs)   = show x <> "," <> commas xs

-- AllF basically enforces the constraint on all members of the 
-- list. 
type AllF :: (a -> Constraint) -> [a] -> Constraint 
type family AllF c xs where 
    AllF c '[]       = ()
    AllF c (x : xs) = (c x, AllF c xs)

-- type All c xs = AllF c xs 

-- class synonym so that constraint can work on higher order constraints.
class AllF c xs => All c xs 
instance AllF c xs => All c xs 


type Product :: (a -> Type) -> [a] -> Type 
data Product f xs where
    Nil  :: Product f '[]
    (:*) :: f x -> Product f xs -> Product f (x : xs)

infixr 5 :* 

-- deriving instance Show (Product f '[])
-- deriving instance (Show (f x), Show (Product f xs)) => Show (Product f (x:xs))
deriving instance All (Compose Show f) xs => Show (Product f xs)


example2 :: Product Identity [Bool, Char, Int]
example2 = Identity True :* Identity 'f' :* Identity 2 :* Nil 



-- type Compose f g x = f (g x)

class f (g x) => Compose f g x
instance f (g x) => Compose f g x

type Dict :: Constraint -> Type 
data Dict c where 
    Dict :: c => Dict c 

toCompose :: Dict (f (g x)) -> Dict (Compose f g x)
toCompose Dict = Dict 

fromCompose :: Dict (Compose f g x) -> Dict (f (g x))
fromCompose Dict = Dict 

example3 :: Product (Product Identity) [[Bool, Char, Int], '[Bool]]
example3 = example2 :* (Identity True :* Nil) :* Nil  

csv :: All (All Show) xss => Product (Product Identity) xss -> String
csv Nil        = "" 
csv (x :* xs) = commas' x <> "\n" <> csv xs 

commas' :: All Show xs => Product Identity xs -> String 
commas' Nil = ""
commas' (Identity x :* Nil) = show x 
commas' (Identity x :* xs)  = show x <> "," <> commas' xs  


-- >>> commas example1
-- "True,1,'f'"

-- >>> 
