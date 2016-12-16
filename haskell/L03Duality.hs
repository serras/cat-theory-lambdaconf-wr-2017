{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language PolyKinds, KindSignatures #-}
{-# language TypeInType #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
module L03Duality where

import Data.Kind
import L01Categories
import L02UniversalConstructions

-- CoProducts are the dual of products
class Category o m => CategoryWithCoProducts o (m :: o -> o -> Type) where
  type CoProduct o m :: o -> o -> o
  inj1  :: m a (CoProduct o m a b)
  inj2  :: m b (CoProduct o m a b)
  (+++) :: m a c -> m b c -> m (CoProduct o m a b) c

-- Dual category in general
newtype Dual m a b = DD (m b a)

instance Category o m => Category o (Dual m) where
  identity = DD identity
  compose (DD x) (DD y) = DD $ compose y x

instance CategoryWithProducts o m => CategoryWithCoProducts o (Dual m) where
  type CoProduct o (Dual m) = Product o m
  inj1 = DD proj1
  inj2 = DD proj2
  (DD f) +++ (DD g) = DD (f &&& g)