{-# LANGUAGE RankNTypes, KindSignatures, GADTs #-}

module Main where

import Control.Monad

data Free f a = Pure a | Roll (f (Free f a))

ins :: (Functor f) => f a -> Free f a
ins fa = Roll (fmap Pure fa)

free :: (Functor f, Monad g) =>
        (forall a. f a -> g a) -> (forall a. Free f a -> g a)
free f (Pure a)  = return a
free f (Roll fa) = join (f (fmap (free f) fa))
