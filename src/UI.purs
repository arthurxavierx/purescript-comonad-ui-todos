module UI where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Comonad.Pairing (move)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Functor.Pairing (class Pairing)
import Data.Identity (Identity)

type Handler base m = base (m Unit) -> base Unit

type UI base m a = Handler base m -> a

type ComponentT base w m a = w (UI base m a)
type Component w m a = ComponentT Identity w m a

explore
  :: forall base w m ui
   . Comonad w
  => Monad base
  => Pairing m w
  => (ComponentT base w m ui -> base Unit)
  -> ComponentT base w m ui
  -> ui
explore write space = extract space send
  where
    send :: Handler base m
    send base = base >>= \m -> write (move space m)

liftUI :: forall base m mp. Monad base => m ~> mp -> UI base m ~> UI base mp
liftUI liftm ui = \send -> ui \m -> send (m >>= pure <<< liftm)

liftUIT :: forall base m mp. Monad base => MonadTrans mp => Monad m => UI base m ~> UI base (mp m)
liftUIT = liftUI lift

liftUIEff :: forall base m mp. Monad base => (forall a. base (m a) -> base (mp a)) -> UI base m ~> UI base mp
liftUIEff liftm ui = \send -> ui (send <<< liftm)

liftComponent
  :: forall base w m mp
   . Functor w
  => Monad base
  => m ~> mp
  -> ComponentT base w m ~> ComponentT base w mp
liftComponent liftm = map (liftUI liftm)

liftComponentT
  :: forall base w m mp
   . Functor w
  => Monad base
  => MonadTrans mp
  => Monad m
  => ComponentT base w m ~> ComponentT base w (mp m)
liftComponentT = map liftUIT

liftComponentEff
  :: forall base w m mp
   . Functor w
  => Monad base
  => (forall a. base (m a) -> base (mp a))
  -> ComponentT base w m ~> ComponentT base w mp
liftComponentEff liftm = map (liftUIEff liftm)

effect
  :: forall base w m a
   . Monad base
  => Comonad w
  => Pairing m w
  => (ComponentT base w m a -> base Unit)
  -> ComponentT base w m a
  -> UI base m a
effect transform component send = extract component \base -> send do
  action <- base
  transform (move component action)
  pure action
