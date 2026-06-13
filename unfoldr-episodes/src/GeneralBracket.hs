{-# LANGUAGE LambdaCase #-}
module GeneralBracket where 

import Control.Exception hiding (bracket)
import Control.Concurrent hiding (withMVar)
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad


-- takeMVar :: MVar a -> IO a 
-- putMVar :: MVar a -> a -> IO ()

-- Asynchronous exceptions can occur at any time, they are 
-- unpredictable, can be caused by anything, including other threads 
-- etc or even user operations or commands like Ctrl-C

{-
withMVar :: MVar a -> (a -> IO b) -> IO b 
withMVar v f = do 
    a <- takeMVar v 
    b <- f a -- What if this function fails and return and exception?
    putMVar v a 
    return b  
-}

{-
withMVar :: MVar a -> (a -> IO b) -> IO b 
withMVar v f = do 
    a <- takeMVar v 
    mb <- try $ f a 
    putMVar v a 
    case mb of 
        Right b -> return b 
        Left  (e :: SomeException) -> throwIO e  

-- But now this code does not account for asynchronous exception
-- If at some point we take the value in MVar and some exception occurs
-- other threads will be block for ever because this does not release the 
-- lock on the MVar

-}

-- The mask function helps to control which asynchronous exception is allowed 
-- when the function is running
-- We unmask ensures that the function `f` can still accept asynchronous exception
-- when it is running, for example when timeout exception is thrown by another thread
withMVar :: MVar a -> (a -> IO b) -> IO b 
withMVar v f = mask $ \ unmask -> do 
    a <- takeMVar v 
    mb <- try $ unmask $ f a 
    putMVar v a 
    case mb of 
        Right b -> return b 
        Left  (e :: SomeException) -> throwIO e  

bracket :: IO a -> (a -> IO ()) -> (a -> IO b) -> IO b 
bracket acquire release use = mask $ \unmask -> do 
    a <- acquire
    mb <- try $ unmask $ use a
    release a 
    case mb of 
        Right b -> return b 
        Left (e :: SomeAsyncException) -> throwIO e 

withMVar' :: MVar a -> (a -> IO b) -> IO b
withMVar' v = bracket (takeMVar v) (putMVar v)  

{-
class Monad m => MonadBracket m where 
    generalBracket :: m a -> (a -> m ()) -> (a -> m b) -> m b 
    -- this signature is too restrictive, we cannnot use it for StateT 
-}

-- generalBracketIO :: IO a  -> (a -> Either SomeException b -> IO c) -> (a -> IO b) -> IO c 
-- generalBracketIO acquire release use = mask $ \unmask -> do 
--     a <- acquire 
--     mb <- try $ unmask $ use a 
--     case mb of 
--         Right b -> release a (Right b)
--         Left  (e :: SomeException) -> do 
--             _ <- release a (Left e)
--             throwIO e 



{-
class Monad m => MonadBracket m where 
    generalBracket :: m a  -> (a -> Either SomeException b -> m c) -> (a -> m b) -> m c 


instance MonadBracket IO where 
    -- generalBracket = bracket 
    generalBracket = generalBracketIO


instance MonadBracket m => MonadBracket (StateT s m) where 
    generalBracket (StateT acquire) release use = StateT $ \s -> 
        generalBracket
        (acquire s) 
        (\ (a, s') -> \case 
             Right (b, s'') -> runStateT (release a (Right b)) s'' 
             Left  e        -> runStateT (release a (Left e)) s' 
        ) 
        (\ (a, s') -> runStateT (use a) s') 

-- instance MonadBracket m => (ExceptT e m) where 
--     generalBracket (ExceptT acquire) release use = ExceptT $ 
--         generalBracket
--           acquire
--           _ 
--           undefined
-- again this does not generalise again into ExceptT 
-}


data ExitCase a = 
      ExistCaseSuccess a 
    | ExitCaseException SomeException 
    | ExistCaseAbort 
    deriving Show 


class Monad m => MonadBracket m where 
    generalBracket :: m a  -> (a -> ExitCase b -> m c) -> (a -> m b) -> m c 


generalBracketIO :: IO a  -> (a -> ExitCase b -> IO c) -> (a -> IO b) -> IO c 
generalBracketIO acquire release use = mask $ \unmask -> do 
    a <- acquire 
    mb <- try $ unmask $ use a 
    case mb of 
        Right b -> release a (ExistCaseSuccess b)
        Left  e -> do 
            void $ release a (ExitCaseException e)
            throwIO e 


instance MonadBracket IO where 
    generalBracket = generalBracketIO 

instance MonadBracket m => MonadBracket (StateT s m) where 
    generalBracket (StateT acquire) release use = StateT $ \s -> 
        generalBracket
          (acquire s)
          (\ (a, s') -> \case 
             ExistCaseSuccess (b, s'') -> runStateT (release a (ExistCaseSuccess b)) s'' 
             ExitCaseException e       -> runStateT (release a (ExitCaseException e)) s'
             ExistCaseAbort            -> runStateT (release a ExistCaseAbort)  s' 
          )
          (\ (a, s') -> runStateT (use a) s')


instance MonadBracket m => MonadBracket (ExceptT e m) where 
    generalBracket (ExceptT acquire) release use = ExceptT $ 
        generalBracket
           acquire 
           (\case 
              Left  e -> const $ return (Left e)
              Right a -> \case 
                ExistCaseSuccess  (Right b) -> runExceptT (release a (ExistCaseSuccess b))
                ExitCaseException e -> runExceptT (release a (ExitCaseException e))
                ExistCaseAbort      -> runExceptT (release a ExistCaseAbort)
                ExistCaseSuccess  (Left e1) -> do 
                    mc <- runExceptT $ release a ExistCaseAbort
                    case mc of 
                        Left e2  -> return $ Left e2 
                        Right _  -> return $ Left e1)
           (\case 
              Right a -> runExceptT (use a) 
              Left  e -> return (Left e)
            ) 
           