{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cell where

import BoundBehavior

import Control.Monad.Except
import Data.Word

instance {-# OVERLAPPABLE #-} (Bounded a, Enum a, Eq a) => Seekable a where
  prev a | a == minBound = throwError SeekMin
  prev a = return $ pred a

  next a | a == maxBound = throwError SeekMax
  next a = return $ succ a

  ismin = (== minBound)
  ismax = (== maxBound)

  makemin _ = minBound
  makemax _ = maxBound

class Cell c where
  decrC :: (MonadError String m) => c -> m c
  incrC :: (MonadError String m) => c -> m c
  toCell :: (MonadError String m) => Int -> m c
  fromCell :: c -> Int

data BoundedCell b c where
  BoundedCell :: BoundBehavior b -> c -> BoundedCell b c

cellErrorMessage SeekMin = "Cannot decrement cell below minimum value"
cellErrorMessage SeekMax = "Cannot increment cell above maximum value"

instance
  (Seekable c, Bounded c, Enum c, Ord c, BoundBehaviorT b) =>
  Cell (BoundedCell b c) where
  decrC (BoundedCell b c) =
    either (throwError . cellErrorMessage) return $
    fmap (BoundedCell b) $ decr b c
  incrC (BoundedCell b c) =
    either (throwError . cellErrorMessage) return $
    fmap (BoundedCell b) $ incr b c

  toCell i | i < (fromEnum (minBound :: c)) =
             throwError "Cannot set cell value below minimum"
  toCell i | i > (fromEnum (maxBound :: c)) =
             throwError "Cannot set cell value above maximum"
  toCell i = return $ BoundedCell mkBB (toEnum i)
  fromCell (BoundedCell _ c) = fromEnum c

defaultCell :: BoundedCell NoWrapT Word8
defaultCell = BoundedCell NoWrap 0
