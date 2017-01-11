{-# language RankNTypes #-}
module Yoneda where

-- Yoneda Lemma

lemma1 :: Functor f => (forall x. (a -> x) -> f x) -> f a
lemma1 yo = yo id

lemma2 :: Functor f => f a -> (forall x. (a -> x) -> f x)
lemma2 fa = \ax -> fmap ax fa

-- Yoneda Lemma on (a ->)

natFun1 :: (forall x. (a -> x) -> (b -> x)) -> b -> a
natFun1 = lemma1

natFun2 :: (b -> a) -> (forall x. (a -> x) -> (b -> x))
natFun2 = lemma2