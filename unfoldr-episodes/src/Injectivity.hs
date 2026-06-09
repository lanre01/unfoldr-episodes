-- {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Injectivity where 

import Data.Kind
import Data.Monoid

{-
a function is injective if there exists a unique 
element in the codomain(image) for every element in 
the domain
-} 

double :: Integer -> Integer
double x = x + x 

-- parametised datatypes are injective by default

data Proxy a = Proxy 
-- Proxy Bool is different from Proxy Int 
-- if we consider just the types
-- Proxy a is injective


showEmpty :: forall a. (Show a, Monoid a) => a -> String
showEmpty _ = show (mempty :: a) 


type family Fam a 
type instance Fam [Bool] = Bool -- This is basically computation on type level
type instance Fam All    = Bool  

-- Fam a is not injective, the language allows the type checking of this
-- showEmpty' :: forall a. (Show a, Monoid a) => Fam a -> String
-- showEmpty' _ = show (mempty :: a) 

-- example = showEmpty' @[Bool] True 

showEmpty'' :: forall a. (Show a, Monoid a) => Proxy a -> Fam a -> String
showEmpty'' _ _ = show (mempty :: a)

example2 :: String
example2 = showEmpty'' (Proxy @[Bool]) False 


class Encryption e where 
  type Key e :: Type 
  type Enc e :: Type -> Type 

  encrypt :: Proxy e -> Key e -> a -> Enc e a 

type family Fam2 a = r | r -> a 
type instance Fam2 [Bool] = Bool 
-- type instance Fam2 All    = Bool 

type family Fam3 a = r 
type instance Fam3 [Bool] = Bool 
type instance Fam3 All    = Bool 

type family Fam3Inv r 
type instance Fam3 Bool = [Bool]

showEmpty''' :: forall a. (Show a, Monoid a, a ~ Fam3Inv (Fam3 a )) => Fam3 a -> String
showEmpty''' _ = show (mempty :: a)


type family Server api :: Type 
data Application 

serve :: ({- ... -}) => Proxy api -> Server api -> Application 
serve = undefined


