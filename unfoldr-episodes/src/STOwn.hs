{-# LANGUAGE DerivingVia #-}
{- HLINT ignore "Use <$>" -}
module STOwn where 

import Control.Monad.State 
import Control.Monad.Identity
import Data.Vector ( Vector )
import qualified Data.Vector as Vector 



newtype ST s a = MkST (Store -> (a, Store))
  deriving (Functor, Applicative, Monad) via State Store 

data Store = 
    MkStore 
     { memory :: !(Vector Int)
     , next   :: !Offset  
     }

type Offset = Int 

newtype STRef s = MkSTRef Offset 

newSTRef :: Int -> ST s (STRef s)
newSTRef i = 
    MkST $ \(MkStore memory next) -> 
        (MkSTRef next, MkStore (Vector.snoc memory i) (next + 1))

readSTRef :: STRef s -> ST s Int 
readSTRef (MkSTRef offset) = 
    MkST $ \st@(MkStore memory _) -> 
        (memory Vector.! offset, st)

writeSTRef' :: STRef s -> Int -> ST s ()
writeSTRef' (MkSTRef offset) !i = 
    MkST $ \(MkStore memory next) -> 
        ((), MkStore (memory Vector.// [(offset, i)]) next)

modifySTRef' :: STRef s -> (Int -> Int) -> ST s ()
modifySTRef' stref f = do 
    val <- readSTRef stref
    writeSTRef' stref (f val)

-- by including the s in the constructor for ST 
-- and using the forall quantification over the first argument
-- this ensures that the s does not depend on a
-- therefore the STRef cannot escape
-- i.e STRef cannot be returned normally outside of an ST computation. 
-- because a must not contain STRef otherwise the forall quantification
-- will fail.
-- runST has the flexibility of choosing the right argument.
runST :: (forall s. ST s a) -> a 
runST (MkST f) = fst (f (MkStore Vector.empty 0))

classify :: [Int] -> (Int, Int)
classify xs = runST $ do 
    neg <- newSTRef 0
    pos <- newSTRef 0
    let 
        step x = case compare x 0 of 
            LT -> modifySTRef' neg (+1)
            GT -> modifySTRef' pos (+1)
            EQ -> pure ()
    mapM_ step xs 
    pure (,) <*> readSTRef neg <*> readSTRef pos 

-- >>> classify [1, 3, 0, -2, 4, -5]
-- (2,3)

-- This is not longer type correct
-- becaue we cannot return STRef
-- otherwise STRef s will escape
-- which fails type check
-- evil :: STRef s  
-- evil = runST $ newSTRef 17
-- the return value here should be MkSTRef 0
-- with the vector containing just 17. 
-- by doing this we have lost access to the vector though? 
-- so if we call this STRef on some other vector it can have problem

-- eviler :: ST s Int 
-- eviler = do 
--     x <- readSTRef evil 
--     writeSTRef' evil (x + 1)
--     return x 
-- this actually fails immediately in the first line
-- why? - evil creates a new vector which stores 17, but we are reading
-- trying to read from an empty vector using writeSTRef'


-- test :: Int 
-- test = runST eviler 

-- >>> test
-- /home/lawal/Code_linux/Haskell/haskell/unfoldr-episodes/src/STOwn.hs:65:16: error: [GHC-25897]
--     • Couldn't match type ‘s1’ with ‘s’
--       Expected: ST s1 (STRef s)
--         Actual: ST s1 (STRef s1)
--       ‘s1’ is a rigid type variable bound by
--         a type expected by the context:
--           forall (s1 :: k). ST s1 (STRef s)
--         at /home/lawal/Code_linux/Haskell/haskell/unfoldr-episodes/src/STOwn.hs:65:16-26
--       ‘s’ is a rigid type variable bound by
--         the type signature for:
--           evil :: forall {k} (s :: k). STRef s
--         at /home/lawal/Code_linux/Haskell/haskell/unfoldr-episodes/src/STOwn.hs:64:1-15
--     • In the second argument of ‘($)’, namely ‘newSTRef 17’
--       In the expression: runST $ newSTRef 17
--       In an equation for ‘evil’: evil = runST $ newSTRef 17
--     • Relevant bindings include
--         evil :: STRef s
--           (bound at /home/lawal/Code_linux/Haskell/haskell/unfoldr-episodes/src/STOwn.hs:65:1)
-- (deferred type error)


{-
the same problem here can be forced on the normal ST 


unsafeRunST :: ST s a -> a 
unsafeRunST m = runST (unsafeCoerce m)




-}
