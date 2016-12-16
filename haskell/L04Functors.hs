{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances, FlexibleContexts #-}
{-# language PolyKinds, KindSignatures #-}
{-# language DataKinds, TypeInType #-}
{-# language TypeFamilies, FunctionalDependencies #-}
{-# language InstanceSigs, UndecidableInstances #-}
module L04Functors where

import Data.Kind
import L01Categories

-- Definition of functor
class (Category o1 m1, Category o2 m2)
  => CFunctor o1 (m1 :: o1 -> o1 -> *) o2 (m2 :: o2 -> o2 -> *)
              (obj :: o1 -> o2) | obj -> o1 m1 o2 m2 where
  cfunctor_m :: m1 a b -> m2 (obj a) (obj b)

-- First example of a functor: lists
{-
instance CFunctor Type (->) Type (->) [] where
  cfunctor_m = map
-}

-- Every `Functor` is a functor from Hask to Hask
instance Functor f => CFunctor Type (->) Type (->) f where
  cfunctor_m = fmap