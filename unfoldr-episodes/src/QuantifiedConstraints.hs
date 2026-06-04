{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}


module QuantifiedConstraints where

import Codec.Borsh hiding (deserialiseBorsh)
-- import Codec.Borsh qualified as Borsh
import Control.Applicative
import Control.Monad
import Control.Monad.Combinators.Expr
-- import Crypto.Cipher.AES qualified as Cryptonite
-- import Crypto.Cipher.Types qualified as Cryptonite
-- import Crypto.Error qualified as Cryptonite
-- import Crypto.Random qualified as Cryptonite
import Data.ByteString qualified as Strict
-- import Data.ByteString.Lazy qualified as Lazy
-- import Data.Functor.Product
import Data.Kind
import Data.Proxy
-- import Data.Type.Equality
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L




-- I actually need to understand couple of things before i start
-- type family
-- GADTs
-- Datakinds
-- Kind indexed GADTs

{-
Kinds are to types what types are to values
Concrete types -> * 
Polymorphic types -> * -> * 
Higher-Kinder Types -> (* -> *) -> *
-}

class (forall m. Monad m => Monad (t m)) => MonadTrans t where 
  lift :: Monad m => m a -> t m a 

newtype Stack t1 t2 m a = Stack (t1 (t2 m) a)
   deriving (Functor, Applicative, Monad)

instance (MonadTrans t1, MonadTrans t2) => MonadTrans (Stack t1 t2) where 
  lift = Stack . lift . lift 


-------------------------------------------------------------------------------

class (forall a. Show (Action state a)) => StateModel (state :: Type) where 
  data Action state :: Type -> Type 

data RestServer -- .. = .. 

instance StateModel RestServer where 
  data Action RestServer a where 
    AddUser      :: String -> Action RestServer Int 
    GetUser      :: Int -> Action RestServer String 
    GetAllUsers  :: Action RestServer [String] 
    -- Id           :: Show a => a -> Action RestServer a 
    -- Enumerate    :: Action RestServer (a -> b) -- if the Show contraint has been enforced on a 
                                               -- then this function will not work. 

deriving instance Show (Action RestServer a)

-- why is it taking state??
showAction :: StateModel state => state -> Action state a -> String 
showAction _ = show 

---------------------------------------------------------------------------------

data Expr a where 
  Val :: a -> Expr a 
  Add :: Expr Int -> Expr Int -> Expr Int 
  Equal :: Expr Int -> Expr Int -> Expr Bool 
  If    :: Expr Bool -> Expr a -> Expr a -> Expr a 

deriving instance Show a => Show (Expr a)

eval :: Expr a -> a 
eval = undefined 

data SomeShowable (f :: Type -> Type) where 
  ExistsShowable :: Show a => f a -> SomeShowable f 

deriving instance (forall a. Show a => Show (f a)) => Show (SomeShowable f) 
 
parseExpr :: Parser (SomeShowable Expr)
parseExpr = undefined 


-------------------------------------------------------------------------------

data AES 
data Mock 

class Eq (Enc e a) => EqEnc e a  
instance Eq (Enc e a) => EqEnc e a 

class (Eq (Key e), forall a. Eq a => EqEnc e a) => Encryption e where 
  type Key e :: Type 
  type Enc e :: Type -> Type 

  encrypt :: ToBorsh a => Proxy e -> Key e -> a -> Enc e a


instance Encryption AES where 
  type Key AES = Strict.ByteString
  type Enc AES = Const Strict.ByteString
  encrypt      = undefined

data MockEnc a = MockEnc Int a deriving Eq 

instance Encryption Mock where 
  type Key Mock = Int 
  type Enc Mock = MockEnc 
  encrypt _     = MockEnc










{-------------------------------------------------------------------------------
  Parsing untyped expressions
-------------------------------------------------------------------------------}

-- | Untyped expressions (the result of parsing)
data UExpr =
    UInt Int
  | UBool Bool
  | UAdd UExpr UExpr
  | UEqual UExpr UExpr
  | UIf UExpr UExpr UExpr
  deriving (Show)



type Parser = Parsec Void String

pUExpr :: Parser UExpr
pUExpr =
    makeExprParser pTerm operatorTable
  where
    pTerm :: Parser UExpr
    pTerm = asum [
          parens pUExpr
        , UInt        <$> lexeme L.decimal
        , UBool True  <$  pKeyword "true"
        , UBool False <$  pKeyword "false"
        , UIf         <$  pKeyword "if"
                      <*> pUExpr
                      <*  pKeyword "then"
                      <*> pUExpr
                      <*  pKeyword "else"
                      <*> pUExpr
        ]

    operatorTable :: [[Operator Parser UExpr]]
    operatorTable = [
          [ binary "+"  UAdd   ]
        , [ binary "==" UEqual ]
        ]

    binary :: String -> (UExpr -> UExpr -> UExpr) -> Operator Parser UExpr
    binary name f = InfixL (f <$ symbol name)

    pKeyword :: String -> Parser ()
    pKeyword keyword = void $
        lexeme (string keyword <* notFollowedBy alphaNumChar)

    parens :: Parser a -> Parser a
    parens = between (symbol "(") (symbol ")")

    -- lexing

    sc :: Parser ()
    sc = L.space space1 empty empty

    lexeme :: Parser a -> Parser a
    lexeme = L.lexeme sc

    symbol :: String -> Parser ()
    symbol = void . L.symbol sc