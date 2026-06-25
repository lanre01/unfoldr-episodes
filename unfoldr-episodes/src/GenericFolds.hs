{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
module GenericFolds where 

-- Recursion Schemes


import Data.Kind 

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving Show 

data TreeF a r = LeafF a | NodeF r r 
    deriving (Show, Functor)


-- newtype TreeF' a r = TreeF' (Either (r, r) a)

-- example' = TreeF' (Right 1)

exampleTree :: Tree Int 
exampleTree = Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))

exampleTreeF :: TreeF Int String 
exampleTreeF = NodeF "Haskell" "Unfolder"

exampleTree3 :: TreeF Int (Tree Int)
exampleTree3 = NodeF (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))

class Functor (FunctorOf t) => Rec t where 
    type FunctorOf t :: Type -> Type 
    
    pull :: t -> FunctorOf t t  
    push :: FunctorOf t t -> t

instance Rec (Tree a) where 
    type FunctorOf (Tree a) = TreeF a 
    pull :: Tree a -> TreeF a (Tree a)
    pull (Leaf x)   = LeafF x 
    pull (Node l r) = NodeF l r 

    push :: TreeF a (Tree a) -> Tree a 
    push (LeafF x)   = Leaf x 
    push (NodeF l r) = Node l r

-- x :: t 
--  pull x :: FunctorOf t t 
-- fmap (fold alg) (pull x) :: FunctorOf t r 
-- alg (fmap (fold alg) (pull x)) :: r
fold :: Rec t =>  (FunctorOf t r -> r) -> t -> r
fold alg = alg . fmap (fold alg) . pull 


toList :: Tree a -> [a]
toList = fold $ \case   
    LeafF x -> [x]
    NodeF l r -> l ++ r 

render :: Show a => Tree a -> String
render = fold $ \case 
     LeafF x    -> show x 
     NodeF l r -> "( " ++ l ++ " | " ++ r ++ " )"

data ListF a r = NilF | ConsF a r 
   deriving (Show, Functor)

instance Rec [a] where 
    type FunctorOf [a] = ListF a 
    pull :: [a] -> ListF a [a]
    pull []  = NilF 
    pull (x : xs) = ConsF x xs

    push :: ListF a [a] -> [a]
    push NilF = []
    push (ConsF x xs) = x : xs 


foldr :: (a -> r -> r) -> r -> [a] -> r 
foldr cons nil = fold $ \case 
       NilF -> nil 
       ConsF x xs -> x `cons` xs  
   
unfold :: Rec t => (r -> FunctorOf t r) -> r -> t
unfold coalg = push . fmap (unfold coalg) . coalg

build :: Int -> a -> Tree a 
build d x = unfold step d 
  where 
    step i | i <= 0 = LeafF x
           | otherwise = NodeF (i - 1) (i - 1) 

-- >>> render $ build 3 'x'
-- "( ( ( 'x' | 'x' ) | ( 'x' | 'x' ) ) | ( ( 'x' | 'x' ) | ( 'x' | 'x' ) ) )"

-- >>> exampleTreeF
-- NodeF "Haskell" "Unfolder"





