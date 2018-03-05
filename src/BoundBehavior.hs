{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module BoundBehavior where

import Control.Monad.Except

data SeekError = SeekMin | SeekMax

class Seekable a where
  next :: (MonadError SeekError m) => a -> m a
  prev :: (MonadError SeekError m) => a -> m a
  ismin :: a -> Bool
  ismax :: a -> Bool
  makemin :: a -> a
  makemax :: a -> a

data NoWrapT
data WrapT
data StopT
class BoundBehaviorT b where
  mkBB :: BoundBehavior b
instance BoundBehaviorT NoWrapT where { mkBB = NoWrap }
instance BoundBehaviorT WrapT where { mkBB = Wrap }
instance BoundBehaviorT StopT where { mkBB = Stop }

data BoundBehavior b where
  NoWrap :: BoundBehavior NoWrapT
  Wrap :: BoundBehavior WrapT
  Stop :: BoundBehavior StopT

decr :: (Seekable a, MonadError SeekError m) => BoundBehavior b -> a -> m a
decr Wrap a | ismin a = return $ makemax a
decr Stop a | ismin a = return a
decr _ a = prev a

incr :: (Seekable a, MonadError SeekError m) => BoundBehavior b -> a -> m a
incr Wrap a | ismax a = return $ makemin a
incr Stop a | ismax a = return a
incr _ a = next a
