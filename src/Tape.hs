{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module Tape where

import BoundBehavior
import Cell

import Control.Monad.Except
import Data.Word

data ListZipper a = ListZipper [a] a [a]

instance Seekable (ListZipper a) where
  prev (ListZipper (x:xs) y ys) = return $ ListZipper xs x (y:ys)
  prev (ListZipper [] _ _) = throwError SeekMin

  next (ListZipper xs x (y:ys)) = return $ ListZipper (x:xs) y ys
  next (ListZipper _ _ []) = throwError SeekMax

  ismin (ListZipper [] _ _) = True
  ismin _ = False

  ismax (ListZipper _ _ []) = True
  ismax _ = False

  makemin (ListZipper xs v ys) =
    let (first:rest) = reverse (v:xs) in
      ListZipper [] first (rest ++ ys)

  makemax (ListZipper xs v ys) =
    let (last:rest) = reverse (v:ys) in
      ListZipper  (rest ++ xs) last []

class (Cell a) => Tape t a | t -> a where
  moveLeft :: (MonadError String m) => t -> m t
  moveRight :: (MonadError String m) => t -> m t
  get :: t -> a
  set :: t -> a -> t
  tapeFromList :: [a] -> t

  incrT :: (MonadError String m) => t -> m t
  incrT t = (incrC $ get t) >>= return . set t
  decrT :: (MonadError String m) => t -> m t
  decrT t = (decrC $ get t) >>= return . set t
  test :: t -> Bool
  test = (== 0) . fromCell . get

data BoundedTape b c where
  BoundedTape :: BoundBehavior b -> ListZipper c -> BoundedTape b c

tapeErrorMessage SeekMin = "Cannot move before first element"
tapeErrorMessage SeekMax = "Cannot move after last element"

instance (Cell c, BoundBehaviorT b) => Tape (BoundedTape b c) c where
  moveLeft (BoundedTape b l) =
    either (throwError . tapeErrorMessage) return $
    fmap (BoundedTape b) $ decr b l
  moveRight (BoundedTape b l) =
    either (throwError . tapeErrorMessage) return $
    fmap (BoundedTape b) $ incr b l
  get (BoundedTape _ (ListZipper _ v _)) = v
  set (BoundedTape b (ListZipper xs _ ys)) v =
    (BoundedTape b (ListZipper xs v ys))
  tapeFromList (l:ls) = BoundedTape mkBB (ListZipper [] l ls)


defaultSize :: Int
defaultSize = 30000

defaultTape :: BoundedTape NoWrapT (BoundedCell NoWrapT Word8)
defaultTape = tapeFromList (take defaultSize $ repeat defaultCell)

defaultInfiniteTape :: BoundedTape NoWrapT (BoundedCell NoWrapT Word8)
defaultInfiniteTape = tapeFromList (repeat defaultCell)
