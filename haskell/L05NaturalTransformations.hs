{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances, FlexibleContexts #-}
{-# language TypeSynonymInstances #-}
{-# language PolyKinds, KindSignatures #-}
{-# language DataKinds, TypeInType #-}
module L05NaturalTransformations where

import Data.Kind
import L01Categories
import L04Functors

-- We cannot be as general as we desire...
class (CFunctor co cm Type em f, CFunctor co cm Type em g)
      => NaturalTransformation co (cm :: co -> co -> Type) (em :: Type -> Type -> Type)
                               (f :: co -> Type) (g :: co -> Type) where
  mu :: f a -> g a

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

instance NaturalTransformation Type (->) (->) [] Maybe where
  mu = safeHead