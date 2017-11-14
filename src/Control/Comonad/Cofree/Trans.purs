module Control.Comonad.Cofree.Trans where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Comonad.Trans.Class (class ComonadTrans)
import Control.Extend (class Extend, extend)
import Data.Bifunctor (bimap)
import Data.Identity (Identity(..))
import Data.Lazy (Lazy, defer, force)
import Data.Tuple (Tuple(..), fst, snd)

type CofreeF f a b = Tuple a (Lazy (f b))

newtype CofreeT f w a = CofreeT (w (CofreeF f a (CofreeT f w a)))

type Cofree f = CofreeT f Identity

cofree :: forall f a. CofreeF f a (Cofree f a) -> Cofree f a
cofree = CofreeT <<< Identity

runCofreeT :: forall f w a. CofreeT f w a -> w (CofreeF f a (CofreeT f w a))
runCofreeT (CofreeT w) = w

coresume :: forall f w a. Comonad w => CofreeT f w a -> w (Tuple a (f (CofreeT f w a)))
coresume (CofreeT w) = w <#> \(Tuple a as) -> Tuple a (force as)

unfoldCofreeT :: forall f w s a. Functor f => Comonad w => (s -> w (Tuple a (f s))) -> s -> CofreeT f w a
unfoldCofreeT f s = CofreeT $ f s <#> map (\next -> defer \_ -> unfoldCofreeT f <$> next)

unfoldCofree :: forall f s a. Functor f => (s -> Tuple a (f s)) -> s -> Cofree f a
unfoldCofree f = unfoldCofreeT (Identity <<< f)

instance eqCofreeT :: Eq (w (CofreeF f a (CofreeT f w a))) => Eq (CofreeT f w a) where
  eq = eq

instance ordCofreeT :: Ord (w (CofreeF f a (CofreeT f w a))) => Ord (CofreeT f w a) where
  compare = compare

instance functorCofreeT :: (Functor f, Functor w) => Functor (CofreeT f w) where
  map f (CofreeT w) = CofreeT $ map (bimap f (map (map (map f)))) w

instance extendCofreeT :: (Functor f, Comonad w) => Extend (CofreeT f w) where
  extend f =
    CofreeT
    <<< extend (\w -> Tuple (f (CofreeT w)) (map (extend f) <$> snd (extract w)))
    <<< runCofreeT

instance comonadCofreeT :: (Functor f, Comonad w) => Comonad (CofreeT f w) where
  extract (CofreeT w) = fst (extract w)

instance comonadTransCofreeT :: ComonadTrans (CofreeT f) where
  lower = map fst <<< runCofreeT
