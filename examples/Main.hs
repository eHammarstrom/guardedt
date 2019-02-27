{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad.Trans
import Control.Monad.Trans.Guarded

-- Constrained IO monad as shown by MightyByte
newtype GuardedContext a = GuardedContext { runGuardedContext :: IO a }
  deriving (Functor, Applicative, Monad)

type ReadGuard s = GuardedT s GuardedContext

gReadFile :: FilePath -> GuardedContext String
gReadFile = GuardedContext . readFile

newtype Token s = Token String

getToken :: ReadGuard s (Token s)
getToken = do
  token <- lift $ gReadFile "./LICENSE"
  return . Token $ token

getUsers :: ReadGuard s [String]
getUsers = do
  token <- getToken
  return []

runReadGuard :: (forall s. ReadGuard s a) -> IO a
runReadGuard f = runGuardedContext $ runGuardedT f

main :: IO ()
main = do

  users <- runReadGuard getUsers
  -- token <- runReadGuard getToken -- error

  print users
  print "hurray for phantoms!"
