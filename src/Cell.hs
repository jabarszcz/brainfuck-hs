{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Brainfuck.Cell where

import Control.Monad.Except
import Data.Word

class Cell c where
  incr :: (MonadError String m) => c -> m c
  decr :: (MonadError String m) => c -> m c
  toCell :: (MonadError String m) => Int -> m c
  fromCell :: c -> Int

data NoWrapCell a = NoWrapCell a deriving (Eq, Ord, Bounded, Show)
data WrapCell a = WrapCell a deriving (Eq, Ord, Bounded, Show)
data StopCell a = StopCell a deriving (Eq, Ord, Bounded, Show)

instance (Enum a, Bounded a, Ord a) => Cell (NoWrapCell a) where
  incr (NoWrapCell c)
    | c == maxBound =
      throwError "Cannot increment cell above maximum value"
  incr (NoWrapCell c) = return . NoWrapCell $ succ c

  decr (NoWrapCell c)
    | c == minBound =
      throwError "Cannot decrement cell below minimum value"
  decr (NoWrapCell c) = return . NoWrapCell $ pred c

  toCell i | i > (fromEnum (maxBound :: a)) =
             throwError "Cannot set cell value above maximum"
  toCell i | i < (fromEnum (minBound :: a)) =
             throwError "Cannot set cell value below minimum"
  toCell i = return $ NoWrapCell (toEnum i)

  fromCell (NoWrapCell c) = fromEnum c

instance (Enum a, Bounded a, Eq a) => Cell (WrapCell a) where
  incr (WrapCell c) | c == maxBound = return $ WrapCell minBound
  incr (WrapCell c) = return . WrapCell $ succ c

  decr (WrapCell c) | c == minBound = return $ WrapCell maxBound
  decr (WrapCell c) = return . WrapCell $ pred c

  toCell i =
    return . WrapCell . toEnum $
    ((fromEnum (minBound :: a)) +
      i `mod` (fromEnum (maxBound :: a) - fromEnum (minBound :: a)))

  fromCell (WrapCell c) = fromEnum c

instance (Enum a, Bounded a, Ord a) => Cell (StopCell a) where
  incr (StopCell c) | c == maxBound = return $ StopCell maxBound
  incr (StopCell c) = return . StopCell $ succ c

  decr (StopCell c) | c == minBound = return $ StopCell minBound
  decr (StopCell c) = return . StopCell $ pred c

  toCell i | i > (fromEnum (maxBound :: a)) = return $ StopCell maxBound
  toCell i | i < (fromEnum (minBound :: a)) = return $ StopCell minBound
  toCell i = return $ StopCell (toEnum i)

  fromCell (StopCell c) = fromEnum c

defaultCell :: NoWrapCell Word8
defaultCell = NoWrapCell 0
