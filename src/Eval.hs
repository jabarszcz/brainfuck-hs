{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Eval where

import Cell
import Program
import Tape

import Control.Monad
import Control.Monad.Except
import Data.Char

class AbstractIO io a where
  send :: a -> io ()
  recv :: io a

instance (Cell a) => AbstractIO (ExceptT String IO) a where
  send = ExceptT . fmap Right . (putChar . chr . fromCell)
  recv = (ExceptT . fmap Right $ getChar) >>= toCell . ord

step :: (Tape a b, MonadError String m, AbstractIO m b) => a -> Op -> m a
step t Incr = incrT t
step t Decr = decrT t
step t MoveLeft = moveLeft t
step t MoveRight = moveRight t
step t Input = recv >>= return . set t
step t Output = send (get t) >> return t
step t (Loop ops) | not (test t) = eval t (ops ++ [Loop ops])
step t _ = return t

eval :: (Tape a b, MonadError String m, AbstractIO m b) => a -> Program -> m a
eval = foldM step

evalIO :: (Tape a b) => a -> Program -> IO a
evalIO t p = runExceptT (eval t p) >>= either fail return
