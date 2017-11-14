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
    send base = base >>= \m -> write (move m space)

liftUI
  :: forall base w m mp
   . Functor w
  => Monad base
  => (m ~> mp)
  -> (ComponentT base w m ~> ComponentT base w mp)
liftUI liftm = map (\ui send -> ui \m -> send (m >>= pure <<< liftm))

liftUIT
  :: forall base w m mp
   . Functor w
  => Monad base
  => MonadTrans mp
  => Monad m
  => ComponentT base w m
  ~> ComponentT base w (mp m)
liftUIT = liftUI lift
