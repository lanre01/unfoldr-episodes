{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE QuantifiedConstraints   #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}


module Main where 


import Data.Functor.Identity 
import Data.Kind 
import Data.Proxy 


data Value (a :: Type) where 
    VInt  :: Int -> Value Int 
    VBool :: Bool -> Value Bool 

value :: Value a -> a 
value (VInt v) = v  
value (VBool v) = v 
 


data Dict (c :: k -> Constraint) (a :: k) where 
    Dict :: c a => Dict c a 

canShowValue :: Value a -> Dict Show a 
canShowValue (VInt _)  = Dict 
canShowValue (VBool _) = Dict



showValue :: Value a -> String 
showValue v = 
    case canShowValue v of 
        Dict -> show (value v)

------------------------------------------------------------------
-- NP Maybe '[Int, Bool, Char] ~= (Maybe Int, Maybe Bool, Maybe Char)

data NP (f :: k -> Type) (as :: [k]) where 
    Nil :: NP f '[]
    Cons :: f a -> NP f as -> NP f (a ': as) 

type family AllF (c :: k -> Constraint) (as :: [k]) :: Constraint where 
    AllF c '[]       = ()
    AllF c (a ': as) = (c a, AllF c as) 


equalNP :: AllF Eq as => NP Identity as -> NP Identity as -> Bool 
equalNP Nil         Nil         = True 
equalNP (Cons x xs) (Cons y ys) = x == y && equalNP xs ys 

class    AllF c as => All c as  
instance AllF c as => All c as 


allNP :: NP (Dict c) as -> Dict (All c) as 
allNP Nil   = Dict 
allNP (Cons Dict ds) = case allNP ds of 
                         Dict -> Dict  


mapNP :: (forall a. f a -> g a) -> NP f as -> NP g as 
mapNP _ Nil  = Nil 
mapNP f (Cons x xs) = Cons (f x) (mapNP f xs)


exampleAll :: NP Value as -> Dict (All Show) as 
exampleAll = undefined


---------------------------------------------------------------
-- Implication 

implies :: (All c as, forall a. c a => d a) => Proxy c -> Proxy d -> NP f as -> Dict (All d) as
implies _  _ Nil = Dict 
implies pc pd (Cons _ xs) = case implies pc pd xs of 
                              Dict -> Dict 

foo :: All Ord as => NP Identity as -> NP Identity as -> Bool 
foo xs ys = 
    case implies (Proxy @Ord) (Proxy @Eq) xs of 
        Dict -> equalNP xs ys 



main :: IO ()
main = putStrLn "Hello World!"