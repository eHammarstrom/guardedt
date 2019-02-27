{-# LANGUAGE MagicHash, UnboxedTuples #-} -- GHC Magic
{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving #-}

module Control.Monad.Trans.Guarded
  ( GuardedT
  , runGuardedT
  )
where

import GHC.Prim
import GHC.Magic

import Control.Monad.Identity
import Control.Monad.Trans.Guarded.Internal

runGuardedT :: Monad m => (forall s. GuardedT s m a) -> m a
runGuardedT gma =
  let x = (unGuardedT gma)
  in case runRW# x of y -> y >>= return . snd

{-
type Guard s a = GuardedT s Identity a

runGuard :: (forall s. Guard s a) -> a
runGuard g = runIdentity $ runGuardedT g
-}
