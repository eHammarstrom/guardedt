{-# LANGUAGE MagicHash #-}

module Control.Monad.Trans.Guarded.Internal
  ( GuardedT(..)
  )
where

import GHC.Prim

import Control.Monad.Trans

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
