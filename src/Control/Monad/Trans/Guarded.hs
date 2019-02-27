{-# LANGUAGE MagicHash, UnboxedTuples #-} -- GHC Magic
{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving #-}

module Guarded where

import GHC.Prim
import GHC.Magic

import Control.Monad.Trans
import Control.Monad.Identity

data BoxedState s = BoxedState (State# s)

unbox :: BoxedState s -> State# s
unbox (BoxedState x) = x

box :: State# s -> BoxedState s
box x = BoxedState x

newtype GuardedT s m a = GuardedT { unGuardedT :: State# s -> m ( BoxedState s, a ) }

instance Monad m => Functor (GuardedT s m) where
  fmap f xs = GuardedT $ \s ->
    let msa = (unGuardedT xs) s
    in  msa >>= \(s', a) -> return (s', f a)

instance Monad m => Applicative (GuardedT s m) where
  pure a = GuardedT $ \s -> return (box s, a)

  gf <*> ga = GuardedT $ \s -> do
    let mf = (unGuardedT gf) s
    (s'  , f) <- mf

    let ma = (unGuardedT ga) (unbox s')
    (s'' , a) <- ma

    return (s'', f a)

instance Monad m => Monad (GuardedT s m) where
  ga >>= fgb = GuardedT $ \s -> do
    let ma = (unGuardedT ga) s
    (s', a) <- ma
    unGuardedT (fgb a) (unbox s')

instance MonadTrans (GuardedT s) where
  lift m = GuardedT $ \s -> m >>= \a -> return (box s, a)

type Guard s a = GuardedT s Identity a

runGuard :: (forall s. Guard s a) -> a
runGuard g = runIdentity $ runGuardedT g

runGuardedT :: Monad m => (forall s. GuardedT s m a) -> m a
runGuardedT gma =
  let x = (unGuardedT gma)
  in case runRW# x of y -> y >>= return . snd

-- Constrained IO monad as shown by MightyByte
newtype GuardedContext a = GuardedContext { runGuardedContext :: IO a }
  deriving (Functor, Applicative, Monad)

type ReadGuard s = GuardedT s GuardedContext

gReadFile :: FilePath -> GuardedContext String
gReadFile = GuardedContext . readFile

newtype Token s = Token String

getToken :: ReadGuard s (Token s)
getToken = do
  token <- lift $ gReadFile "./superSecretPassword"
  return . Token $ token

getUsers :: ReadGuard s [String]
getUsers = do
  token <- getToken
  return []

runReadGuard :: (forall s. ReadGuard s a) -> IO a
runReadGuard f = runGuardedContext $ runGuardedT f

testGuardedT :: IO ()
testGuardedT = do

  users <- runReadGuard getUsers
  -- token <- runReadGuard getToken -- error

  print users
  print "hurray for phantoms!"

