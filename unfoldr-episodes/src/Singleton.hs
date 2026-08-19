{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

module Singleton where 


data Nat where 
    Zero :: Nat 
    Suc  :: Nat -> Nat

-- Now we create a singleton for Nat 
-- 1-1 correspondence between types of kind Nat and 
-- terms of SNat is why SNat is a Singleton type
data SNat n where 
    SZero :: SNat Zero 
    SSuc  :: SNat n -> SNat (Suc n)
-- So now by pattern matching on SNat, we get some information
-- about the type argument of the SNat 


one :: Nat
one = Suc Zero

type One   = Suc Zero 
type Two   = Suc One 
type Three = Suc Two 
type Four  = Suc Three 
type Five  = Suc Four 


data Vec n a where 
    Nil  :: Vec Zero a 
    (:.) :: a -> Vec n a -> Vec (Suc n) a

infixr 5 :. 
deriving stock instance Show a => Show (Vec n a)

-- This function is fine to define
vzipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c 
vzipWith _ Nil Nil = Nil 
vzipWith f (x :. xs) (y :. ys) = f x y :. vzipWith f xs ys 

-- But defining replicate is difficult 
-- Two problems here :
-- Nat is on the term level not on the type level
-- No connection between "Nat" and n - the function basically 
-- states we can generate an arbitrary vector length from any Nat 

-- vreplicate :: forall (n :: Nat) -> a -> Vec n a 
-- vreplicate Zero _ = Nil 
-- vreplicate (Suc n) x = x :. vreplicate n x 

class Replicate n where 
    vreplicateC :: a -> Vec n a  

-- >>> :t vreplicateC
-- vreplicateC :: Replicate n => a -> Vec n a

instance Replicate Zero where 
    vreplicateC _ = Nil 

instance Replicate n => Replicate (Suc n) where 
    vreplicateC x = x :. vreplicateC x 
-- This implementation does work but the issue is we 
-- leak implementation details which can make using the function
-- down the line unnecessary complex.

-- >>> vreplicateC @Two 'x' 
-- 'x' :. ('x' :. Nil)

-- >>> vzipWith (,) (1 :. 2 :. 3 :. Nil) (vreplicateC 'x')
-- (1,'x') :. ((2,'x') :. ((3,'x') :. Nil))


-- Why does vzipWith work
-- Because we are pattern matching on the constructors of the vectors
-- each constructor gives on some information of the type of the constructors


vreplicateS :: SNat n -> a -> Vec n a 
vreplicateS SZero _  = Nil 
vreplicateS (SSuc n) x = x :. vreplicateS n x 

-- >>> vreplicateS (SSuc (SSuc SZero)) 'x' 
-- 'x' :. ('x' :. Nil)


class SNatI n where 
    sNat :: SNat n 

instance SNatI Zero where 
    sNat = SZero 

instance SNatI n => SNatI (Suc n) where 
    sNat  = SSuc sNat 

vreplicateSI :: SNatI n => a -> Vec n a 
vreplicateSI = vreplicateS sNat


-- >>> vreplicateSI 'x' :: Vec Five Char
-- 'x' :. ('x' :. ('x' :. ('x' :. ('x' :. Nil))))
