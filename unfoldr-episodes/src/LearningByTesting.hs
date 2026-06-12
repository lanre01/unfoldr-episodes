{-# LANGUAGE DerivingVia #-}
{- HLINT ignore "Use lambda-case" -}
module LearningByTesting where 

import Control.Applicative
import Control.Monad.Trans.State
import Prelude hiding (filter)

-- This episode just go over how to use the power types 
-- to write safe programs instead of just using booleans.
-- https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/ 
-- talks about using types to control the control flow of the program


-- | Original filter
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x : xs) = 
    if f x 
        then x : filter f xs 
        else filter f xs
 
-- | The original filter can be improved in two ways
-- 1. specify what is being filtered using types
-- 2. change the type of the result to make errors 
--    difficult

filter' :: (a -> Maybe b) -> [a] -> [b]
filter' _ [] = []
filter' f (x : xs) = 
    case f x of 
        Just y  -> y : filter' f xs 
        Nothing -> filter' f xs 

  
-- Another example 
checkEmail :: String -> Bool 
checkEmail email = '@' `elem` email 

-- The problem with this is that it does give
-- enough information about the result of the check
-- You can imagine doing this check several times 
-- again in other part of the code.

newtype Email = MkEmail String deriving Show 

checkEmail' :: String -> Maybe Email
checkEmail' txt = 
    if '@' `elem` txt 
        then Just $ MkEmail txt 
        else Nothing

-- The new definition gives that certainty that
-- any computation after this has a valid email
-- so no need for checking again


-- Another example 
-- partition function 

partition :: (a -> Bool) -> [a] -> ([a], [a])
-- partition _ []       = ([], [])
partition p = foldr step ([], [])
    where 
        -- step :: a -> ([a], [a]) -> ([a], [a]) ignoring this otherwise i need to add 
        -- quantified constraint (forall a) in the partition type definition.
        step a (ys, zs) = 
            if p a 
                then (a : ys, zs)
                else (ys, a : zs)

    -- let 
    --     (ys, zs) = partition p xs 
    -- in 
    --     if p x 
    --         then (x : ys, zs)
    --         else (ys, x : zs)  


partition' :: (a -> Either b c) -> [a] -> ([b], [c])
partition' f = foldr step ([], []) 
      where 
        step a (ys, zs) = 
            case f a of 
                Right x -> (ys, x : zs)
                Left  x -> (x : ys, zs)


-- data Token a where 
--     Keyword   :: String -> Token String 
--     Identfier :: String -> Token String 
--     Literal   :: Int -> Token Int
--     deriving Show 

data Token where 
    Keyword   :: String -> Token
    Identfier :: String -> Token
    Literal   :: Int -> Token
    deriving Show 


newtype Parser a = MkParser { unParser :: [Token] -> [(a, [Token])] }
  deriving (Functor, Applicative, Monad, Alternative)
    via StateT [Token] []
-- need to understand how it derives

lit :: Parser Int 
lit = (\ x -> case x of 
                Literal i -> i
                _         -> error "ugh" )
                <$> 
                satisfy (\x -> case x of 
                                 Literal _ -> True 
                                 _         -> False  )


lit' :: Parser Int  
lit' = token $ \ x -> 
                 case x of 
                    Literal i -> Just i 
                    _         -> Nothing 

token :: (Token -> Maybe a) -> Parser a 
token p = MkParser $ \tok -> 
    case tok of 
        [] -> []
        (t : ts) -> case p t of 
                      Nothing -> []
                      Just a  -> [(a, ts)]

satisfy :: (Token -> Bool) -> Parser Token
satisfy p = MkParser $ \ tok ->
  case tok of
    [] -> []
    (t : ts)
      | p t -> [(t, ts)]
      | otherwise -> []

runParser :: Parser a -> [Token] -> Maybe a 
runParser p toks = 
    case filter' (\ (r, tok) -> if null tok then Just r else Nothing) (unParser p  toks) of 
        []       -> Nothing
        (x : _)  -> Just x 