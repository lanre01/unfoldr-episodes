{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}

module HigherKindedTypes where 


-- import Data.Functor.Const 
import Data.Functor.Identity
import Data.Text (Text)
-- import Data.Coerce


data Episode f = 
    MkEpisode 
    { number   :: f Int 
    , title    :: f Text 
    , abstract :: f Text 
    , level    :: f Level 
    }
deriving instance (forall a. Show a => Show (f a)) => Show (Episode f)

-- >>> ep14
-- MkEpisode {number = Identity 14, title = Identity "Higher-minded types", abstract = Identity "In this episode", level = Identity Beginner}

data Level = Beginner | Intermediate | Advanced 
  deriving Show 

ep14 :: Episode Identity
ep14 = 
    MkEpisode
     (Identity 14)
     (Identity "Higher-minded types")
     (Identity "In this episode")
     (Identity Beginner)

ep14Update :: Episode Maybe 
ep14Update = 
    MkEpisode 
      Nothing 
      (Just "Higher-Kinded Types")
      Nothing
      (Just Intermediate)

emptyEpisode :: Episode Maybe 
emptyEpisode = pureEpisode Nothing 
    -- MkEpisode
    --   Nothing
    --   Nothing
    --   Nothing
    --   Nothing

updateEpisode :: Episode Identity -> Episode Maybe -> Episode Maybe 
updateEpisode = undefined 

pureEpisode :: (forall a. f a) -> Episode f 
pureEpisode x = 
    MkEpisode
      x 
      x 
      x 
      x 

-- applicative lawaas
-- class Functor f => Applicative f where 
-- pure :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b 



-- class HApplicative where 
-- hpure :: (forall a. f a) -> h f 
-- happ :: h (f :~>: g) -> h f -> h g 

newtype (f :~>: g) a = Fun { apply :: f a -> g a }

apEpisode :: Episode (f :~>: g) -> Episode f -> Episode g 
apEpisode (MkEpisode f1 f2 f3 f4) (MkEpisode x1 x2 x3 x4) = 
    MkEpisode 
      (apply f1 x1)
      (apply f2 x2)
      (apply f3 x3)
      (apply f4 x4)
 
class HApplicative h where 
    hpure :: (forall a. f a) -> h f
    happ :: h (f :~>: g) -> h f -> h g   

instance HApplicative Episode where 
    hpure = pureEpisode
    happ  = apEpisode


update :: HApplicative h => h Identity -> h Maybe -> h Identity 
update original new = 
    hpure aux' --(convert aux)
     `happ` original
     `happ` new 

     where 
        -- convert :: (a -> Maybe a -> a) -> (Identity :~>: (Maybe :~>: Identity)) a 
        -- convert = coerce 
        
        -- aux :: a -> Maybe a -> a 
        -- aux o Nothing  = o 
        -- aux _ (Just n) = n

        aux' :: (Identity :~>: (Maybe :~>: Identity)) a
        aux' = 
            Fun $ \o -> 
                Fun $ \case 
                   Nothing -> o
                   Just n  -> Identity n  

update' :: forall h k. HApplicative h => h Identity -> h k -> (forall a. k a -> Identity a -> Identity a) -> h Identity 
update' original new f = 
    hpure aux 
      `happ` original 
      `happ` new  
    where 
        aux :: (Identity :~>: (k :~>: Identity)) a
        aux = Fun $ \o -> 
                Fun $ \k -> f k o    
-- >>> update ep14 ep14Update
-- MkEpisode {number = Identity 14, title = Identity "Higher-Kinded Types", abstract = Identity "In this episode", level = Identity Intermediate}

-- :~>: Identity (Maybe :~>: Identity)

