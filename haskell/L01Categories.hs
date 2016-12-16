{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances, TypeSynonymInstances #-}
{-# language PolyKinds, KindSignatures #-}
{-# language DataKinds, ConstraintKinds, TypeInType #-}
{-# language GADTs, RankNTypes #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
module L01Categories where

import Data.Kind

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