{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances, TypeSynonymInstances #-}
{-# language PolyKinds, KindSignatures #-}
{-# language TypeInType #-}
{-# language TypeFamilies #-}
module L02UniversalConstructions where

import Data.Kind
import L01Categories

class Category o m => CategoryWithProducts o (m :: o -> o -> Type) where
  type Product o m :: o -> o -> o
  proj1 :: m (Product o m a b) a
  proj2 :: m (Product o m a b) b
  (&&&) :: m c a -> m c b -> m c (Product o m a b)

instance CategoryWithProducts Type (->) where
  type Product Type (->) = (,)
  proj1 = fst
  proj2 = snd
  f &&& g = \x -> (f x, g x)

-- Two products
class Category o m => CategoryWithProducts' o (m :: o -> o -> Type) (p :: o -> o -> o) where
  proj1' :: m (p a b) a
  proj2' :: m (p a b) b
  (&&&&) :: m c a -> m c b -> m c (p a b)

-- Let's prove uniqueness!
productIsUnique :: (CategoryWithProducts' o m p1, CategoryWithProducts' o m p2)
                => m (p1 a b) (p2 a b)
productIsUnique = proj1' &&&& proj2'