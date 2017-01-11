{-# language MultiParamTypeClasses, FunctionalDependencies #-}
{-# language FlexibleInstances, TypeSynonymInstances #-}
{-# language PolyKinds, KindSignatures #-}
{-# language DataKinds, ConstraintKinds, TypeInType #-}
{-# language GADTs, RankNTypes #-}
{-# language TypeOperators #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
module Day2 where

import Data.Kind
import Day1

-- 6. MONOIDAL THINGS
-- ==================

class Category o m => MonoidalCategory o (m :: o -> o -> Type) where
  type Unit  o m :: o
  type Times o m :: o -> o -> o

  assoc1   :: m (Times o m a (Times o m b c)) (Times o m (Times o m a b) c)
  assoc2   :: m (Times o m (Times o m a b) c) (Times o m a (Times o m b c))
  idleft1  :: m (Times o m (Unit o m) a) a
  idleft2  :: m a (Times o m (Unit o m) a)
  idright1 :: m (Times o m a (Unit o m)) a
  idright2 :: m a (Times o m a (Unit o m))

instance MonoidalCategory Type (->) where
  type Unit  Type (->) = ()
  type Times Type (->) = (,)
  assoc1 (a,(b,c)) = ((a,b),c)
  assoc2 ((a,b),c) = (a,(b,c))
  idleft1  ((),a)  = a
  idleft2  a       = ((),a)
  idright1 (a,())  = a
  idright2 a       = (a,())

class MonoidalCategory o m => MonoidObject o (m :: o -> o -> Type) (x :: o) where
  zero :: m (Unit o m) x
  mult :: m (Product o m x x) x

instance Monoid m => MonoidObject Type (->) m where
  zero ()    = mempty
  mult (x,y) = x `mappend` y

-- 7. MONADS
-- =========

-- 8. ALGEBRAS
-- ===========

type Algebra f a = f a -> a
newtype Fix f = Fix { unFix :: f (Fix f) }

cata :: Functor f => Algebra f a -> Fix f -> a
cata alg (Fix x) = alg $ fmap (cata alg) x
