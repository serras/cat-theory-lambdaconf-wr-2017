{-# language MultiParamTypeClasses, FunctionalDependencies #-}
{-# language FlexibleInstances, TypeSynonymInstances #-}
{-# language PolyKinds, KindSignatures #-}
{-# language DataKinds, ConstraintKinds, TypeInType #-}
{-# language GADTs, RankNTypes #-}
{-# language TypeOperators #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
module Day1 where

import Data.Kind

-- 1. CATEGORIES
-- =============

-- Definition of a category
class Category o (m :: o -> o -> Type) where
  identity :: m a a
  compose  :: m b c -> m a b -> m a c

-- Category Hask
instance Category Type (->) where
  identity = id
  compose  = (.)

-- Category from a monoid
data Single = One
data MonoidMorph m (a :: Single) (b :: Single) where
  MM :: m -> MonoidMorph m a b

instance Monoid m => Category Single (MonoidMorph m) where
  identity = MM mempty
  compose (MM x) (MM y) = MM (y `mappend` x)

-- Category from a preorder
class Preorder e (lt :: e -> e -> Type) where
  reflexivity :: a `lt` a
  transivity  :: a `lt` b -> b `lt` c -> a `lt` c

{-
instance Preorder e lt => Category e lt where
  identity = reflexivity
  compose  = flip transivity
-}

-- Category of Haskell ground constraints
newtype ConstraintMorph c d
  = ConstraintMorph { run :: forall x. (c => x) -> (d => x) }

instance Category Constraint ConstraintMorph where
  identity = ConstraintMorph id
  compose bc ab  -- We need (a => x) -> (c => x)
    = ConstraintMorph $ \a -> run bc (run ab a)

-- More usual definition
class Category' c where
  identity' :: c a a
  compose'  :: c b d -> c a b -> c a d


-- 2. UNIVERSAL CONSTRUCTIONS
-- ==========================

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


-- 3. DUALITY
-- ==========

-- CoProducts are the dual of products
class Category o m => CategoryWithCoProducts o (m :: o -> o -> Type) where
  type CoProduct o m :: o -> o -> o
  inj1  :: m a (CoProduct o m a b)
  inj2  :: m b (CoProduct o m a b)
  (+++) :: m a c -> m b c -> m (CoProduct o m a b) c

-- Dual category in general
newtype Op m a b = Op (m b a)

instance Category o m => Category o (Op m) where
  identity = Op identity
  compose (Op x) (Op y) = Op $ compose y x

-- Category with products => Dual has coproducts
instance CategoryWithProducts o m => CategoryWithCoProducts o (Op m) where
  type CoProduct o (Op m) = Product o m
  inj1 = Op proj1
  inj2 = Op proj2
  (Op f) +++ (Op g) = Op (f &&& g)


-- 4. FUNCTORS
-- ===========

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

{-
-- Every `Functor` is a functor from Hask to Hask
instance Functor f => CFunctor Type (->) Type (->) f where
  cfunctor_m = fmap
-}

-- Going back to Hask category

class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

newtype Compose' f g a = Compose' (f (g a))

instance (Functor' f, Functor' g) => Functor' (Compose' f g) where
  fmap' f (Compose' x) = Compose' $ fmap' (fmap' f) x


-- 5. NATURAL TRANSFORMATIONS
-- ==========================

type NatTransf f g = forall a. f a -> g a