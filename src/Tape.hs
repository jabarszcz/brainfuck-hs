{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Brainfuck.Tape where

import Brainfuck.Cell

import Control.Monad.Except
import Data.Word

class (Cell a) => Tape t a | t -> a where
  moveR :: (MonadError String m) => t -> m t
  moveL :: (MonadError String m) => t -> m t
  get :: t -> a
  set :: (MonadError String m) => t -> a -> m t
  tapeFromList :: [a] -> t

  incrT :: (MonadError String m) => t -> m t
  incrT t = (incr $ get t) >>= set t
  decrT :: (MonadError String m) => t -> m t
  decrT t = (decr $ get t) >>= set t
  test :: t -> Bool
  test = (== 0) . fromCell . get

data NoWrapTape a = NoWrapTape [a] a [a]
data WrapTape a = WrapTape [a] a [a]

instance (Cell a) => Tape (NoWrapTape a) a where
  moveR (NoWrapTape pre val (newval:post)) =
    return $ NoWrapTape (val:pre) newval post
  moveR (NoWrapTape _ _ []) = throwError "Cannot move past right end of tape"

  moveL (NoWrapTape (newval:pre) val post) =
    return $ NoWrapTape pre newval (val:post)
  moveL (NoWrapTape [] _ _) = throwError "Cannot move past left end of tape"

  get (NoWrapTape _ val _) = val

  set (NoWrapTape pre _ post) val =
    return $ NoWrapTape pre val post

  tapeFromList (l:ls) =
    NoWrapTape [] l ls

instance (Cell a) => Tape (WrapTape a) a where
  moveR (WrapTape pre val (newval:post)) =
    return $ WrapTape (val:pre) newval post
  moveR (WrapTape pre val []) =
    let (newval:newpost) = reverse (val:pre) in
      return $ WrapTape [] newval newpost

  moveL (WrapTape (newval:pre) val post) =
    return $ WrapTape pre newval (val:post)
  moveL (WrapTape [] val post) =
    let (newval:newpre) = reverse (val:post) in
      return $ WrapTape [] newval newpre

  get (WrapTape _ val _) = val

  set (WrapTape pre _ post) val =
    return $ WrapTape pre val post

  tapeFromList (l:ls) =
    WrapTape [] l ls

defaultSize :: Int
defaultSize = 30000

defaultTape :: NoWrapTape (NoWrapCell Word8)
defaultTape = tapeFromList (take defaultSize $ repeat defaultCell)

defaultInfiniteTape :: NoWrapTape (NoWrapCell Word8)
defaultInfiniteTape = tapeFromList (repeat defaultCell)
