{-# LANGUAGE DataKinds, GADTs, TypeFamilies, AllowAmbiguousTypes,
    TypeApplications #-}


module Vector where 

import Prelude (Show)
import Data.Type.Equality ((:~:) (..))
import Unsafe.Coerce (unsafeCoerce)


data Nat where 
    Zero :: Nat 
    Succ :: Nat -> Nat 

data Vec n a where 
    Nil  :: Vec Zero a 
    (:>) :: a -> Vec n a -> Vec (Succ n) a 

infixr 5 :> 

deriving instance Show a => Show (Vec n a)

data SNat n where 
    SZero :: SNat Zero 
    SSucc :: SNat n -> SNat (Succ n)

type (+) :: Nat -> Nat -> Nat 
type family a + b where
    Zero + b = b 
    Succ a + b = Succ (a +  b) 

snoc :: Vec n a -> a -> Vec (Succ n) a 
snoc Nil a = a :> Nil 
snoc (x :> xs) a = x :> xs `snoc` a 

mPlusZero :: forall m. SNat m -> (m + Zero) :~: m 
mPlusZero SZero = Refl
mPlusZero (SSucc m) = case mPlusZero m of Refl -> Refl
{-# NOINLINE mPlusZero #-}
{-# RULES "mPlusZero" forall m. mPlusZero m = unsafeCoerce Refl #-}

mPlusSucc :: forall n m. SNat m -> (m + Succ n) :~: Succ (m + n)
mPlusSucc SZero = Refl 
mPlusSucc (SSucc m') = case mPlusSucc @n m' of Refl -> Refl
{-# NOINLINE mPlusSucc #-}
{-# RULES "mPlusSucc" forall m. mPlusSucc m = unsafeCoerce Refl #-}

reverse1 :: Vec n a -> Vec n a 
reverse1 Nil = Nil 
reverse1 (x :> xs) = reverse1 xs `snoc` x 

reverse :: Vec n a -> Vec n a
reverse = go SZero Nil 
      where 
        go :: SNat m -> Vec m a -> Vec n a -> Vec (m + n) a 
        go m acc Nil = case mPlusZero m of Refl -> acc 
        go m acc (x :> (xs :: Vec p_pred a)) = case mPlusSucc @p_pred m of Refl -> go (SSucc m) (x :> acc) xs 
